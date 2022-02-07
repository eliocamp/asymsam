writeLines(c(sysreqs::sysreqs("DESCRIPTION", "linux-x86_64-ubuntu-gcc"),
             "libmagick++-dev"), # needed but not discovered, for some reason
           "system-deps.txt")
