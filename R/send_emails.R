#......................
# dependencies
#......................
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(gmailr))
suppressPackageStartupMessages(library(httr))


#......................
# Import Data
#......................
# Have to manually downlaod csv -- lame
dt <- readr::read_csv(file = "~/Downloads/IDEEL Secret Santa 2018 .csv") %>%
  magrittr::set_colnames(tolower(colnames(.)))

#----------------------------------------------------------------------------------------------------
# Make random sample
#----------------------------------------------------------------------------------------------------

secret_santa <-function(names){
  names <- names[!duplicated(names)]
  n <- length(names)
  ret <- tibble(from = names, to = rep(NA, n))

  while(any(ret$from == ret$to) | any(is.na(ret$to))){
    ret$to <- sample(names, n, replace = F)
  }

  return(ret)
}


assignments <- secret_santa(dt$name) %>%
  magrittr::set_colnames(., c("name", "give")) %>%
  left_join(., dt, by = "name") %>%
  select(-c("timestamp")) %>%
  magrittr::set_colnames(., c("name", "give", "email"))

#----------------------------------------------------------------------------------------------------
# Send out emails
#----------------------------------------------------------------------------------------------------

# following this tutorial -- thanks Jenny Bryan -- https://github.com/jennybc/send-email-with-r
use_secret_file("ideelsecretsanta2018.json")

# email items
subject <- "IDEEL Secret Santa"
email_sender <- 'Nick Brazeau <nbrazeau1@gmail.com>' # your Gmail address
body <- "Hi %s,

Your assignment for the IDEEL secret santa is %s.

Thanks for participating in this fun event!

For the record, Santa is obviously an acronym for signature anonymous non-traditional alias (or as Molly would like: super awesome nachos taste amazing).

Thanks and all the best,
Secret Elves

"

# collapse text for emails
out <- assignments %>%
  mutate(
    To = sprintf('%s <%s>', name, email),
    From = email_sender,
    Subject = sprintf(subject),
    body = sprintf(body, name, give)) %>%
  select(To, From, Subject, body)


# make emails
emails <- out %>%
  purrr::pmap(mime)

# send emails
safe_send_message <- safely(send_message)
sent_mail <- emails %>%
  map(safe_send_message)
