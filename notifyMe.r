# This function takes a subject and a body and sends an email to jeradhoy@gmail.com when the script is done running - currently email forwards it to phone
library(mailR)
notifyMe <- function(subject= "Job Finished", body=" "){
    send.mail(from = "jeradhoy@gmail.com",
	      to = "jeradhoy@gmail.com",
	      subject = paste("R -", subject),
	      body = paste(body, Sys.time()),
	      smtp = list(host.name = "aspmx.l.google.com", port = 25),
	      authenticate = FALSE,	 
	      send = TRUE)
}
