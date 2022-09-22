#########################################
### RAFFLE WITH MULTIPLE TICKET TYPES ###
#########################################

### Gregory Wada ###
## Originally written for 2021 Tsukimi Festival

## This set of function draws n number wins from a pool of 'tickets' supplied in a CSV with columns for
## "timestamp", name", "contact", "ticket_type," and "n_ticket". Tickets are added to the pool with
## either raffle_enter, which can be used to import initial entries from a CSV or append new entries from a 
## CSV with just the new entries. Raffle tickets are drawn from the pool with raffle_draw, which removes the 
## winning ticket from the pool and adds it to a new pool of winning tickets. Sequential raffle_draws can be run 
## and the pools (data frames) in the global environment will be updated. The pool of tickets is called
## 'tickets' and the pool of winners is called 'winners.' Drawings that only consider tickets of a certain
## type can be made with raffle_draw_class, which will update the pools as above, but will only draw tickets
## that match the specified class. 

## (1) raffle_enter adds tickets from specified CSV file. It generates an object 'tickets,' which is the pool of eligible tickets.
#### raffle_enter("/Users/wadalab/Downloads/raffle_entries.csv")

## (2) If 'tickets' already exists, raffle_enter adds entries from CSV to existing pool. This CSV should not contain old entries.
#### raffle_enter("/Users/wadalab/Downloads/more_raffle_entries.csv")

## (3) raffle_draw draws n number tickets from pool and can optionally be assigned a prize. Default prize is "unspecified".
#### raffle_draw(tickets,2,"Lucky Cat")

## (4) raffle_draw_class works as above but only considers tickets of specified type when drawing. 
#### raffle_draw_class(tickets,2,"Lucky Cat","Stamps (Goshuin)")
#### "Stamps (Goshuin)" is set as default. raffle_draw_class(tickets,2,"Lucky Cat") will run same as above.

raffle_enter <- function(file) {
   if(!exists("tickets")) {
    entries <- read.csv(file)
    tickets <- entries[rep(seq_len(nrow(entries)),entries$n_tickets),]
    tickets <<- tickets[,-5]
    print("Ticket pool generated as object 'tickets'", quote=FALSE)
    noquote(paste(nrow(tickets),"tickets added."))
    
    } else {
    new_entries <- read.csv(file)
    new_tickets <- new_entries[rep(seq_len(nrow(new_entries)),new_entries$n_tickets),]
    new_tickets <- new_tickets[,-5]
    tickets <<- rbind(tickets,new_tickets)
    print("New tickets added to object 'tickets'", quote=FALSE)
    noquote(paste(nrow(new_tickets),"tickets added."))
  }
}

raffle_draw <- function(tickets,wins,prize="unspecified") {
  tickets$ticket_no <- c(1:nrow(tickets)) 
  draws <- sample(c(1:nrow(tickets)),wins)
  new_winners <- tickets[draws,]
  new_winners <- new_winners[,-5]
  new_winners$prize <- prize
  tickets <<- tickets[-draws,]
  
  if(exists("winners")) {
    print("Appending winners table")
    winners <<- rbind(winners,new_winners)
  } else {
    print("Creating winners table")
    winners <<- new_winners
  }
  
  return(as.data.frame(new_winners))
}

raffle_draw_class <- function(tickets,wins,prize="unspecified",type="Stamps (Goshuin)") {
  holds <- tickets[which(tickets$ticket_type != type),]
  temp_pool <-tickets[which(tickets$ticket_type == type),]
  
  temp_pool$ticket_no <- c(1:nrow(temp_pool)) 
  draws <- sample(c(1:nrow(temp_pool)),wins)
  new_winners <- temp_pool[draws,]
  new_winners <- new_winners[,-5]
  new_winners$prize <- prize
  temp_pool <- temp_pool[-draws,]
  
  if(exists("winners")) {
    print("Appending winners table")
    winners <<- rbind(winners,new_winners)
  } else {
    print("Creating winners table")
    winners <<- new_winners
  }
  
  tickets <<- rbind(holds,temp_pool[,-5])
}


### Additional code, likely not necessary

## (5) raffle_parse removes tickets of a certain class from "tickets," and returns removed tickets. 
## This can be used to remove whole classes of tickets and write them into new data frames. This could be
## used to parse a 'tickets' pool by type to have separate drawings. Or, if the type column is used by
## volunteers to validate entries, it could be used to remove spam or spurious entries. 
#### tickets_donation <- raffle_parse(tickets,"Donation")
#### This example will remove tickets of class "Donation" from "tickets" and write them to "tickets_donation"


raffle_parse <- function(tickets,type) {
  parsed <<- tickets[which(tickets$ticket_type==type),]
  tickets <<- tickets[which(tickets$ticket_type!=type),]
  return(parsed)
}

## (6) raffle_draw_special is an older version of a method to only select winning tickets of a given class. 
## It only works for one drawing at a time and requires a re-run if the ticket is of the wrong class. 
## Slated for removal, but kept as backup. It simulates a more human process of returning tickets, but
## this function is less preferable to raffle_draw_class.

raffle_draw_special <- function(tickets,prize="unspecified",type="Stamps (Goshuin)") {
  tickets$ticket_no <- c(1:nrow(tickets)) 
  draws <- sample(c(1:nrow(tickets)),1)
  new_winners <- tickets[draws,]
  
  if(new_winners$ticket_type == type){
    new_winners$prize <- prize
    tickets <<- tickets[-draws,]
    
    if(exists("winners")) {
      print("Appending winners table")
      new_winners <- new_winners[,-5]
      winners <<- rbind(winners,new_winners)
    } else {
      print("Creating winners table")
      new_winners <- new_winners[,-5]
      winners <<- new_winners
    }
    
    return(as.data.frame(new_winners))
  } else {
    print("Oops, drew a ticket of wrong type. Returning to pool. Please draw again.")
  }
}