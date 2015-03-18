//----------------------------------------------------------------------------------------------------
// Nerd shell
//
// Provides a REPL (Read-Evaluate-Print-Loop) environment
//----------------------------------------------------------------------------------------------------

#include <nerd-int.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <csignal>

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#include <crtdbg.h>
#endif

// Enables the unit testing if set to 1
#define NE_TEST			1
#define NE_BREAK_ALLOC  0

#if NE_TEST
void TestMain();
#endif

static void Output(Nerd N, const char* text)
{
#ifdef _WIN32
    OutputDebugStringA(text);
#endif
    fprintf(stdout, "%s", text);
}

//----------------------------------------------------------------------------------------------------
// Running a single file
//----------------------------------------------------------------------------------------------------

static NeBool RunFile(Nerd N, const char* fileName)
{
    FILE* f;
    size_t length = 0;
    char* code = 0;

    f = fopen(fileName, "rb");
    if (f)
    {
        NeValue result = 0;
        NeString resultString;
        NeBool success;

        if (!fseek(f, 0, SEEK_END)) {
            length = ftell(f);
            fseek(f, 0, SEEK_SET);
        }

        code = (char *)malloc(length + 1);
        fread(code, length, 1, f);
        fclose(f);
        code[length] = 0;

        success = NeRun(N, fileName, code, length + 1, &result);
        resultString = NeToString(N, result, success ? NE_CONVERT_MODE_REPL : NE_CONVERT_MODE_NORMAL);
        NeOut(N, success ? "==> %s\n" : "ERROR: %s\n", resultString);
        free(code);
        if (!success) return NE_NO;
    }
    else
    {
        NeOut(N, "Unable to open %s.", fileName);
    }

    NeGarbageCollect(N);

    return NE_YES;
}

//----------------------------------------------------------------------------------------------------
// Command line analysis
//----------------------------------------------------------------------------------------------------

void RunCommandLine(int argc, char** argv, Nerd T)
{
    int i = 0;

    for (i = 1; i < argc; ++i)
    {
        char* command = argv[i];

        RunFile(T, command);
    }
}

//----------------------------------------------------------------------------------------------------
// GetLine implementation for Win32
//----------------------------------------------------------------------------------------------------

#ifdef WIN32

#include <limits.h>
#include <stdlib.h>
#include <errno.h>

#ifndef SIZE_MAX
# define SIZE_MAX ((size_t) -1)
#endif
#ifndef SSIZE_MAX
# define SSIZE_MAX ((ssize_t) (SIZE_MAX / 2))
#endif
#if !HAVE_FLOCKFILE
# undef flockfile
# define flockfile(x) ((void) 0)
#endif
#if !HAVE_FUNLOCKFILE
# undef funlockfile
# define funlockfile(x) ((void) 0)
#endif

typedef size_t ssize_t;

ssize_t getdelim(char **lineptr, size_t *n, int delimiter, FILE *fp)
{
    ssize_t result;
    size_t cur_len = 0;

    if (lineptr == NULL || n == NULL || fp == NULL)
    {
        errno = EINVAL;
        return -1;
    }

    flockfile(fp);

    if (*lineptr == NULL || *n == 0)
    {
        *n = 120;
        *lineptr = (char *)malloc(*n);
        if (*lineptr == NULL)
        {
            result = -1;
            goto unlock_return;
        }
    }

    for (;;)
    {
        int i;

        i = getc(fp);
        if (i == EOF)
        {
            result = -1;
            break;
        }

        /* Make enough space for len+1 (for final NUL) bytes.  */
        if (cur_len + 1 >= *n)
        {
            size_t needed_max =
                SSIZE_MAX < SIZE_MAX ? (size_t)SSIZE_MAX + 1 : SIZE_MAX;
            size_t needed = 2 * *n + 1;   /* Be generous. */
            char *new_lineptr;

            if (needed_max < needed)
                needed = needed_max;
            if (cur_len + 1 >= needed)
            {
                result = -1;
                goto unlock_return;
            }

            new_lineptr = (char *)realloc(*lineptr, needed);
            if (new_lineptr == NULL)
            {
                result = -1;
                goto unlock_return;
            }

            *lineptr = new_lineptr;
            *n = needed;
        }

        (*lineptr)[cur_len] = i;
        cur_len++;

        if (i == delimiter)
            break;
    }
    (*lineptr)[cur_len] = '\0';
    result = cur_len ? cur_len : result;

unlock_return:
    funlockfile(fp);
    return result;
}

ssize_t getline(char **lineptr, size_t *n, FILE *stream)
{
    return getdelim(lineptr, n, '\n', stream);
}

#endif

//----------------------------------------------------------------------------------------------------
// Creating a session
//----------------------------------------------------------------------------------------------------

static Nerd CreateSession(int argc, const char** argv, NeBool* interactive)
{
    NeConfig config;

    // Create the Nerd virtual machine
    NeSetConfigToDefault(&config);
    config.mCallbacks.mOutputCallback = &Output;

    Nerd N = NeOpen(&config);
    *interactive = NE_NO;

    if (N)
    {
        // Load in 'system.n' in the current directory.
        if (!RunFile(N, "system.n"))
        {
            fprintf(stderr, "ERROR: System file did not execute!\n");
        }
        // Load in the buffers and read them
        int i = 0;
        NeBool success = NE_YES;

        for (i = 1; i < argc; ++i)
        {
            const char* param = argv[i];

            if (*param == '-')
            {
                // Check the flags
                while (*++param)
                {
                    switch (*param)
                    {
                    case 'i':	*interactive = NE_YES; break;
                    case 'e':
                        {
                            // Execute code
                            ++i;
                            if (i < argc)
                            {
                                NeValue result;
                                NeBool success = NeRun(N, "<cmdline>", argv[i], -1, &result);
                                NeString resultString = NeToString(N, result, NE_CONVERT_MODE_REPL);

                                fprintf(stdout, success ? "==> %s\n" : "ERROR: %s\n", resultString);
                            }
                            else
                            {
                                fprintf(stderr, "ERROR: No code provided on command line for execution.\n");
                            }
                        }
                    }
                }
            }
            else if (success && *param == '@')
            {
                // Manifest list
                const char* manifestFileName = param + 1;
                char fileName[1024] = { 0 };
                size_t lengthPrefix = 0;
                FILE* file;

                strncpy(fileName, manifestFileName, 1023);
                lengthPrefix = strlen(fileName);
                while (lengthPrefix &&
                    fileName[lengthPrefix - 1] != '/' &&
                    fileName[lengthPrefix - 1] != '\\')
                {
                    --lengthPrefix;
                }
                fileName[lengthPrefix] = 0;

                file = fopen(manifestFileName, "rt");
                if (file)
                {
                    char* readline = 0;
                    size_t readlineLen = 0;

                    while ((readlineLen = getline(&readline, &readlineLen, file)) > 0)
                    {
                        if (-1 == (int)readlineLen)
                        {
                            free(readline);
                            break;
                        }

                        if (readline[0] == '#') continue;

                        // Read a file name from the manifest file
                        if (readline[readlineLen - 1] == '\n')
                        {
                            --readlineLen;
                            readline[readlineLen] = 0;
                            if (0 == readlineLen)
                            {
                                free(readline);
                                break;
                            }
                        }

                        strncpy(fileName + lengthPrefix, readline, 1023 - lengthPrefix);
                        free(readline);
                        readline = 0;

                        // We have the filename of the script to read in.
                        success = RunFile(N, fileName);
                        if (!success) break;
                    }

                    fclose(file);
                }
                else
                {
                    fprintf(stderr, "ERROR: Cannot open manifest file '%s'.\n", manifestFileName);
                }
            }
            else if (success)
            {
                success = RunFile(N, param);
            }
        }
    }

    return N;
}

//----------------------------------------------------------------------------------------------------
// Main entry point
//----------------------------------------------------------------------------------------------------

void SigHandler(int)
{

}

int main(int argc, const char** argv)
{
    Nerd N;
    NeBool interactive = NE_NO;

#if NE_BREAK_ALLOC > 0 && defined(_WIN32)
    _CrtSetBreakAlloc(NE_BREAK_ALLOC);
#endif

    std::signal(SIGINT, &SigHandler);

    // Initialise the session
    N = CreateSession(argc, argv, &interactive);
    if (argc == 1) interactive = NE_YES;

    // The main loop
    if (N)
    {
#if NE_TEST
        TestMain();
#endif

        if (interactive)
        {
            fprintf(stdout, "Nerd Shell (V" NE_VERSION_STRING ")\n");
            fprintf(stdout, NE_COPYRIGHT_STRING "\n\n");
#if _WIN32
            fprintf(stdout, "Type CTRL-C or enter ,q to quit.\n\n");
#else
            fprintf(stdout, "Type CTRL-D or enter ,q to quit.\n\n");
#endif

            for (;;)
            {
                char* input = 0;
                size_t size = 0;
                char* nspace = 0;
                ssize_t result = 0;

                fprintf(stdout, "%s> ", nspace ? nspace : "/");

                result = getline(&input, &size, stdin);
                if ((-1 != result) && (*input == ','))
                {
                    switch (input[1])
                    {
                    case 'q':
                        result = -1;
                        break;
                    }
                }
                if (-1 == result)
                {
                    free(input);
                    break;
                }

                if (size)
                {
                    NeValue result = 0;
                    NeBool success = NeRun(N, "<stdin>", input, size, &result);
                    NeString resultString = NeToString(N, result, success ? NE_CONVERT_MODE_REPL : NE_CONVERT_MODE_NORMAL);

                    fprintf(success ? stdout : stderr, success ? "==> %s\n" : "ERROR: %s\n", resultString);
                }

                free(input);

                NeGarbageCollect(N);
            }
        }

        NeClose(N);
    }

    fprintf(stdout, "\nEND\n");

#if defined(_WIN32) && defined(_DEBUG)
    if (!_CrtDumpMemoryLeaks())
    {
        OutputDebugStringA("NO MEMORY LEAKS!\n");
    }
#endif
    return 0;
}
