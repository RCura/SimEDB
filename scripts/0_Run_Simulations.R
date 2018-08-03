library(processx)

setwd("/home/robin/gama_18git/headless")
experimentPath="/home/robin/SimFeodal/experiments/expe_5_0_T_07-08.xml"

outputPath="/home/robin/myOutputs/sim"

create_commandLine <- function(num_core){
  sprintf("taskset -c %s,%s bash gama-headless.sh -m 8192m %s %s%s", num_core, num_core+1,
          experimentPath, outputPath,
          num_core)
}

gama_run1 <- process$new(commandline = create_commandLine(0))
gama_run2 <- process$new(commandline = create_commandLine(2))
gama_run3 <- process$new(commandline = create_commandLine(4))
gama_run4 <- process$new(commandline = create_commandLine(6))
gama_run5 <- process$new(commandline = create_commandLine(8))
gama_run6 <- process$new(commandline = create_commandLine(10))
gama_run7 <- process$new(commandline = create_commandLine(12))
gama_run8 <- process$new(commandline = create_commandLine(14))
gama_run9 <- process$new(commandline = create_commandLine(16))
gama_run10 <- process$new(commandline = create_commandLine(18))

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
  gama_run10$is_alive()
  ))) {
  print(Sys.time())
  Sys.sleep(30)
}
print(sprintf("Fini à %s", Sys.time()))
