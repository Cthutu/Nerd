// Unit tests
//

#include <nerd-int.h>
#include <string.h>
#include <stdio.h>

typedef struct _TestInfo
{
    Nerd		mSession;
    NeUInt		mPassedTests;
    NeUInt		mFailedTests;
    NeUInt      mWarnings;
}
TestInfo, *TestInfoRef;

//----------------------------------------------------------------------------------------------------
// Testing functions
//----------------------------------------------------------------------------------------------------

#define PASS ++T->mPassedTests
#define FAIL ++T->mFailedTests

static void DisplayTest(TestInfoRef T, const char* testName, const char* code)
{
    NeOut(T->mSession, "--------------------------------------------------- %s\n", testName);
    NeOut(T->mSession, "CODE: %s\n", code);
}

NeBool DisplayError(TestInfoRef T, const char* testName, const char* code, const char* error)
{
    DisplayTest(T, testName, code);
    NeOut(T->mSession, "TEST FAILED: [%s] %s\n", testName, error);
    return NE_NO;
}

NeBool DisplayExpectError(TestInfoRef T, const char* testName)
{
    NeOut(T->mSession, "[PASS] %s\n", testName);
    return NE_YES;
}

NeBool TestCode(TestInfoRef T, const char* testName, const char* code, NeBool expectToFail)
{

    if (!NeRun(T->mSession, "<Test>", code, -1))
    {
        NeString reason = NePopString(T->mSession);
        if (expectToFail)
        {
            return DisplayExpectError(T, testName);
        }
        else
        {
            return DisplayError(T, testName, code, reason);
        }
    }
    else
    {
        if (!NeToString(T->mSession, -1, NE_CONVERT_MODE_REPL))
        {
            return DisplayError(T, testName, code, "Unable to convert result to string.");
        }

        if (expectToFail)
        {
            return DisplayError(T, testName, code, "This test should have failed!");
        }
    }

    return NE_YES;
}

NeBool TestSuccess(TestInfoRef T, const char* testName, const char* code)
{
    return TestCode(T, testName, code, NE_NO);
}

NeBool TestFailed(TestInfoRef T, const char* testName, const char* code)
{
    return TestCode(T, testName, code, NE_YES);
}

void TestEqualString(TestInfoRef T, const char* testName, const char* code, const char* result)
{
    if (TestSuccess(T, testName, code))
    {
        NeString codeResult = NePopString(T->mSession);
        if (!strcmp(result, codeResult))
        {
            NeOut(T->mSession, "[PASS] %s\n", testName);
            PASS;
        }
        else
        {
            DisplayError(T, testName, code, "Results do not match!");
            NeOut(T->mSession, "RESULT: %s\n", codeResult);
            NeOut(T->mSession, "EXPECTED: %s\n", result);
            FAIL;
        }
    }
    else
    {
        FAIL;
    }
}

void TestFail(TestInfoRef T, const char* testName, const char* code)
{
    if (TestFailed(T, testName, code))
    {
        PASS;
    }
    else
    {
        FAIL;
    }
}

//----------------------------------------------------------------------------------------------------
// Tests
//----------------------------------------------------------------------------------------------------

void TestMain()
{
    TestInfo testInfo;
    TestInfoRef T;
    FILE* f;

    testInfo.mSession = 0;
    testInfo.mPassedTests = 0;
    testInfo.mFailedTests = 0;
    testInfo.mWarnings = 0;
    T = &testInfo;

    if ((f = fopen("core.tt", "r")))
    {
        NeUInt line = 0;
        // Read in the tests
        // The format of each line can be one of 4 formats:
        //
        //      - Starts with a # (ignore line)
        //      - Name(no spaces) <tab or tabs> Code <tab or tabs> Result <newline>
        //      - Empty line (ignore line)
        //      - Other (warning)
        //
        while (!feof(f))
        {
            char buffer[2048];
            char nameBuffer[256];
            char code[1024];
            char result[256];
            const char* name = nameBuffer;

            T->mSession = NeOpen(0);
            if (!T->mSession)
            {
                fprintf(stderr, "ERROR: Failed to create a Titan session.");
                return;
            }

            ++line;
            if (fgets(buffer, 2048, f))
            {
                const char* namei = buffer, *codei, *resulti;
                const char* namee, *codee, *resulte;
                const char* scan = buffer;

                if (*scan == '#' || *scan == '\n')
                {
                    NeClose(T->mSession);
                    continue;
                }

                // Extract name
                while (*scan && (*scan != '\t')) ++scan;
                namee = scan;
                if (!*scan)
                {
                    NeOut(T->mSession, "WARNING:%llu: Invalid line format\n", line);
                    ++T->mWarnings;
                    NeClose(T->mSession);
                    continue;
                }
                while (*scan && (*scan == '\t')) ++scan;
                strncpy(nameBuffer, namei, (size_t)(namee - namei));
                nameBuffer[namee - namei] = 0;

                // Extract code
                codei = scan;
                while (*scan && (*scan != '\t')) ++scan;
                codee = scan;
                if (!*scan)
                {
                    NeOut(T->mSession, "WARNING:%llu: [%s] Invalid line format\n", line, name);
                    ++T->mWarnings;
                    NeClose(T->mSession);
                    continue;
                }
                while (*scan && (*scan == '\t')) ++scan;
                strncpy(code, codei, (size_t)(codee - codei));
                code[codee - codei] = 0;

                // Extract result
                resulti = scan;
                while (*scan && (*scan != '\n')) ++scan;
                resulte = scan;
                if (!*scan)
                {
                    NeOut(T->mSession, "WARNING:%llu: [%s] Invalid line format\n", line, name);
                    ++T->mWarnings;
                    NeClose(T->mSession);
                    continue;
                }
                strncpy(result, resulti, (size_t)(resulte - resulti));
                result[resulte - resulti] = 0;

                if (*name == '-')
                {
                    NeOut(T->mSession, "BREAK\n");
                    ++name;
                }

                if (strcmp(result, "FAIL") == 0)
                {
                    TestFail(T, name, code);
                }
                else
                {
                    TestEqualString(T, name, code, result);
                }
            }

            NeClose(T->mSession);
        }
    }

    //
    // Wrap up
    //
    fprintf(stdout, "--------------------------------------------------------------------------\n");
    fprintf(stdout, "TESTS: %llu tests ran, %llu passed, %llu failed, %llu warnings.\n",
        testInfo.mPassedTests + testInfo.mFailedTests,
        testInfo.mPassedTests,
        testInfo.mFailedTests,
        testInfo.mWarnings);
    fprintf(stdout, "--------------------------------------------------------------------------\n");
}