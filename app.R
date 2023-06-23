NewTrading="C:\\Users\\aldoh\\Documents\\NewTrading\\"
options(encoding = "UTF-8")

### TRUE, FALSE or an environment, determining where the parsed expressions are evaluated.
# FALSE (the default) corresponds to the user's workspace (the global environment)
# and TRUE to the environment from which source is called.

### Also source will parse the entire file and once every line is parsed
### then it will execute the content of the source file
source("helpersv2.R",local={if (interactive()) F else T})
runApp(launch.browser = TRUE)
