library(processx)

gamaHeadlessPath <- "/data/user/c/rcura/gama_30ae578b6_20181119/headless/"

modelFile <- "/data/user/c/rcura/SimFeodal/models/Base5_GUI.gaml"
experimentPath="/data/user/c/rcura/SimFeodal/experiments/expe_5_0_Debug.xml"
file.edit(modelFile)
file.edit(experimentPath)

outputPath="/data/user/c/rcura/myOutputs/sim"

create_commandLine <- function(num_core){
  sprintf("taskset -c %s,%s bash %s -m 8192m %s %s%s", num_core, num_core+1,
          gamaHeadlessPath,
          experimentPath, outputPath, 
          num_core)
}

create_commandLine(0)
foo <- c("-c", "0-1", "bash", "gama-headless.sh",
         "-m", "8192m", "/data/user/c/rcura/SimFeodal/experiments/expe_5_0_Debug.xml",
         "/data/user/c/rcura/myOutputs/sim0")

test <- process$new(command = "taskset", args = foo, wd = gamaHeadlessPath,
                    echo_cmd = TRUE, stdout = "stdout.txt", stderr = "stderr.txt", )
gama_run1 <- process$new(create_commandLine(0))
gama_run2 <- process$new(commandline = create_commandLine(2))
gama_run3 <- process$new(commandline = create_commandLine(4))
gama_run4 <- process$new(commandline = create_commandLine(6))
gama_run5 <- process$new(commandline = create_commandLine(8))
gama_run6 <- process$new(commandline = create_commandLine(10))
gama_run7 <- process$new(commandline = create_commandLine(12))
gama_run8 <- process$new(commandline = create_commandLine(14))
gama_run9 <- process$new(commandline = create_commandLine(16))
gama_run10 <- process$new(commandline = create_commandLine(18))
gama_run11 <- process$new(commandline = create_commandLine(20))
gama_run12 <- process$new(commandline = create_commandLine(22))
gama_run13 <- process$new(commandline = create_commandLine(24))
gama_run14 <- process$new(commandline = create_commandLine(26))
gama_run15 <- process$new(commandline = create_commandLine(28))
gama_run16 <- process$new(commandline = create_commandLine(30))
gama_run17 <- process$new(commandline = create_commandLine(32))
gama_run18 <- process$new(commandline = create_commandLine(34))
gama_run19 <- process$new(commandline = create_commandLine(36))
gama_run20 <- process$new(commandline = create_commandLine(38))

print(sprintf("Début à %s", Sys.time()))
while (all(c(
  gama_run1$is_alive(),
  gama_run2$is_alive(),
  gama_run3$is_alive(),
  gama_run4$is_alive(),
  gama_run5$is_alive(),
  gama_run6$is_alive(),
  gama_run7$is_alive(),
  gama_run8$is_alive(),
  gama_run9$is_alive(),
  gama_run10$is_alive(),
  gama_run11$is_alive(),
  gama_run12$is_alive(),
  gama_run13$is_alive(),
  gama_run14$is_alive(),
  gama_run15$is_alive(),
  gama_run16$is_alive(),
  gama_run17$is_alive(),
  gama_run18$is_alive(),
  gama_run19$is_alive(),
  gama_run20$is_alive()
  ))) {
  print(Sys.time())
  Sys.sleep(30)
}
print(sprintf("Fini à %s", Sys.time()))
