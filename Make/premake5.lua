-- Nerd build script

rootdir = path.join(path.getdirectory(_SCRIPT), "..")

filter { "platforms:Win32" }
	system "Windows"
	architecture "x32"

filter { "platforms:Win64" }
	system "Windows"
	architecture "x64"


-- Solution
solution "Nerd"
	language "C"
	configurations { "Debug", "Release" }
	platforms { "Win32", "Win64" }
	location "../_Build"
    debugdir "../Data"

	defines {
		"_CRT_SECURE_NO_WARNINGS"
	}

    editandcontinue "off"

    rtti "off"
    exceptionhandling "off"

	configuration "Debug"
		defines { "_DEBUG" }
		flags { "FatalWarnings" }
		symbols "on"

	configuration "Release"
		defines { "NDEBUG" }
		flags { "FatalWarnings" }
		optimize "full"

	-- Projects
	project "nd"
		language "C++"
		targetdir "../_Bin/%{cfg.platform}_%{cfg.buildcfg}_%{prj.name}"
		objdir "../_Obj/%{cfg.platform}_%{cfg.buildcfg}_%{prj.name}"
		kind "ConsoleApp"
		files {
			"../Src/nd/**.h",
			"../Src/nd/**.cc",
			"../Data/**.*"
		}
		links {
			"Nerd"
		}
		includedirs {
			"../Src/Nerd"
		}
		postbuildcommands {
			"copy \"" .. path.translate(path.join(rootdir, "Data", "*.*")) .. '" "' ..
				path.translate(path.join(rootdir, "_Bin", "%{cfg.platform}_%{cfg.buildcfg}_%{prj.name}")) .. '"'
				--path.translate(path.join(rootdir, "_Build")) .. '"'
		}


	project "Nerd"
		targetdir "../_Bin/%{cfg.platform}_%{cfg.buildcfg}_%{prj.name}"
		objdir "../_Obj/%{cfg.platform}_%{cfg.buildcfg}_%{prj.name}"
		kind "StaticLib"
		files {
			"../Src/Nerd/**.h",
			"../Src/Nerd/**.c"
		}

