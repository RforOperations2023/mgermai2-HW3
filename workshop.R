library(shiny)
library(leaflet)
library(dplyr)
library(maps)
library(jsonlite)
library(st)
require(sf)
require(leaflet)
require(leaflet.extras)
require(tidyjson)
require(dplyr)
require(readxl)
require(stringr)
require(tidyverse)
library(RColorBrewer)



weddingGuests <- st_read("data/guests.geojson")

input = list(
  guestTypeSelect = c("attended_wedding", "in_wedding_party"),
  guestStateSelect = c("Michigan", "Pennsylvania"),
  guestGenerationSelect = c("Millennial", "Baby Boomer")
)


weddingGuests %>%
  filter(generation %in% input$guestGenerationSelect) %>%
  group_by(generation) %>%
  arrange(generation) %>%
  summarize(n = n()) %>%
  rename("num_guests" = n)









weddingGuests %>%
  # filter(state %in% input$guestStateSelect) %>%
  # select(state) %>%
  # group_by(state, generation) %>%
  group_by(state) %>%
  arrange(state) %>%
  summarize(n = n()) %>%
  rename("num_guests" = n)



dhat <- weddingGuests %>%
  filter(state %in% input$guestStateSelect) %>%
  filter()
  # select(state) %>%
  group_by(state) %>%
  arrange(state) %>%
  summarize(n = n()) %>%
  rename("num_guests" = n)


print(dhat$state)

# this works!!
ggplot(
  data = dhat,
  mapping = aes(
    x = state,
    y = num_guests,
    fill = state
  )
) +
labs(
  x = "State of Residence",
  y = "Number of Guests",
  fill = "State of Residence"
) +
geom_bar(
  alpha = 0.5,
  stat = "identity",
  position = position_dodge()
) +
scale_fill_brewer(palette = "Set1") + 
ggtitle(paste(str_interp("Something something title"))) +
theme(plot.title = element_text(hjust = 0.5))


  # ggplot(
  #   dhat,
  #   mapping = aes(
  #     # fill = generation,
  #     x = state,
  #     y = num_guests
  #   ) +
  #     geom_bar(
  #       position = "dodge",
  #       stat = "identity"
  #     )
  # )


weddingGuestsInputs <- reactive({
  
  weddingGuests <- weddingGuests
  
  # new stuff here
  
  # https://stackoverflow.com/questions/49851381/empty-a-data-frame-keep-colnames-headers-only
  guests_to_be_plotted <- weddingGuests[FALSE, ]
  
  everyone <- weddingGuests
  couple <- subset(weddingGuests, is_bride == TRUE | is_groom == TRUE)
  invited <- subset(weddingGuests, invited == TRUE)
  attendedWedding <- subset(weddingGuests, attended_wedding == TRUE)
  bridesSide <- subset(weddingGuests, inviter == "Eyre")
  groomsSide <- subset(weddingGuests, inviter == "Germaine")
  couplesSide <- subset(weddingGuests, inviter == "Couple")
  weddingParty <- subset(weddingGuests, in_wedding_party == TRUE)
  bridesFamily <- subset(weddingGuests, is_family == TRUE & inviter == "Eyre")
  groomsFamily <- subset(weddingGuests, is_family == TRUE & inviter == "Germaine")
  family <- subset(weddingGuests, is_family == TRUE)
  friends <- subset(weddingGuests, is_friend == TRUE)
  familyFriends <- subset(weddingGuests, is_family_friend == TRUE)
  preCollegeFriends <- subset(weddingGuests, is_pre_college_friend == TRUE)
  collegeFriends <- subset(weddingGuests, is_college_friend == TRUE)
  postCollegeFriends <- subset(weddingGuests, is_post_college_friend == TRUE)
  attendedRehearsalDinner <- subset(weddingGuests, attended_rehearsal_dinner == TRUE)
  attendedWelcomeParty <- subset(weddingGuests, attended_welcome_party == TRUE)
  # fix this later and correct the data as well ("heard_from" instead of "heardFrom") so that it's more uniform.
  heardFrom <- subset(weddingGuests, heardFrom == TRUE)
  # isOfficiant <- subset(weddingGuests, is_officiant == TRUE)
  isVendor <- subset(weddingGuests, is_vendor == TRUE | is_officiant == TRUE)
  
  
  if ("everyone" %in% input$guestTypeSelect) {
    guests_to_be_plotted <- rbind(guests_to_be_plotted, everyone)
  }
  if ("is_couple" %in% input$guestTypeSelect) {
    guests_to_be_plotted <- rbind(guests_to_be_plotted, couple)
  }
  if ("invited" %in% input$guestTypeSelect) {
    guests_to_be_plotted <- rbind(guests_to_be_plotted, invited)
  }
  if ("attended_wedding" %in% input$guestTypeSelect) {
    guests_to_be_plotted <- rbind(guests_to_be_plotted, attendedWedding)
  }
  if ("brides_side" %in% input$guestTypeSelect) {
    guests_to_be_plotted <- rbind(guests_to_be_plotted, bridesSide)
  }
  if ("grooms_side" %in% input$guestTypeSelect) {
    guests_to_be_plotted <- rbind(guests_to_be_plotted, groomsSide)
  }
  if ("couples_side" %in% input$guestTypeSelect) {
    guests_to_be_plotted <- rbind(guests_to_be_plotted, couplesSide)
  }
  if ("wedding_party" %in% input$guestTypeSelect) {
    guests_to_be_plotted <- rbind(guests_to_be_plotted, weddingParty)
  }
  if ("brides_family" %in% input$guestTypeSelect) {
    guests_to_be_plotted <- rbind(guests_to_be_plotted, bridesFamily)
  }
  if ("grooms_family" %in% input$guestTypeSelect) {
    guests_to_be_plotted <- rbind(guests_to_be_plotted, groomsFamily)
  }
  if ("family" %in% input$guestTypeSelect) {
    guests_to_be_plotted <- rbind(guests_to_be_plotted, family)
  }
  if ("friends" %in% input$guestTypeSelect) {
    guests_to_be_plotted <- rbind(guests_to_be_plotted, friends)
  }
  if ("family_friends" %in% input$guestTypeSelect) {
    guests_to_be_plotted <- rbind(guests_to_be_plotted, familyFriends)
  }
  if ("pre_college_friends" %in% input$guestTypeSelect) {
    guests_to_be_plotted <- rbind(guests_to_be_plotted, preCollegeFriends)
  }
  if ("college_friends" %in% input$guestTypeSelect) {
    guests_to_be_plotted <- rbind(guests_to_be_plotted, collegeFriends)
  }
  if ("post_college_friends" %in% input$guestTypeSelect) {
    guests_to_be_plotted <- rbind(guests_to_be_plotted, postCollegeFriends)
  }
  if ("attended_rehearsal_dinner" %in% input$guestTypeSelect) {
    guests_to_be_plotted <- rbind(guests_to_be_plotted, attendedRehearsalDinner)
  }
  if ("attended_welcome_party" %in% input$guestTypeSelect) {
    guests_to_be_plotted <- rbind(guests_to_be_plotted, attendedWelcomeParty)
  }
  if ("heard_from" %in% input$guestTypeSelect) {
    guests_to_be_plotted <- rbind(guests_to_be_plotted, heardFrom)
  }
  if ("is_vendor" %in% input$guestTypeSelect) {
    guests_to_be_plotted <- rbind(guests_to_be_plotted, isVendor)
  }
  
  # https://stackoverflow.com/questions/13967063/remove-duplicated-rows
  noDuplicateGuests <- guests_to_be_plotted[!duplicated(guests_to_be_plotted), ]
  
  # end of new stuff
  
  return(noDuplicateGuests)
  
})



dhat <- weddingGuests %>%
  filter(state %in% input$guestStateSelect) %>%
  # select(state) %>%
  group_by(state) %>%
  arrange(state) %>%
  summarize(n = n()) %>%
  rename("num_guests" = n) %>%
  ggplot(
    dhat,
    mapping = aes(
      # fill = generation,
      x = state,
      y = num_guests
    ) +
    geom_bar(
      position = "dodge",
      stat = "identity"
    )
)




print(class(dhat))
    # group_by(state)
    # summarize(n = n()) %>%
    # rename("num_dogs" = n)
  # print("---dhat"); return(result);








print(input$type[1])

print(length(input$type))


weddingGuestsInputs <- reactive({
  
  weddingGuests <- weddingGuests
  
  # new stuff here
  
  # https://stackoverflow.com/questions/49851381/empty-a-data-frame-keep-colnames-headers-only
  guests_to_be_plotted <- weddingGuests[FALSE, ]
  
  everyone <- weddingGuests
  couple <- subset(weddingGuests, is_bride == TRUE | is_groom == TRUE)
  invited <- subset(weddingGuests, invited == TRUE)
  attendedWedding <- subset(weddingGuests, attended_wedding == TRUE)
  bridesSide <- subset(weddingGuests, inviter == "Eyre")
  groomsSide <- subset(weddingGuests, inviter == "Germaine")
  couplesSide <- subset(weddingGuests, inviter == "Couple")
  weddingParty <- subset(weddingGuests, in_wedding_party == TRUE)
  bridesFamily <- subset(weddingGuests, is_family == TRUE & inviter == "Eyre")
  groomsFamily <- subset(weddingGuests, is_family == TRUE & inviter == "Germaine")
  family <- subset(weddingGuests, is_family == TRUE)
  friends <- subset(weddingGuests, is_friend == TRUE)
  familyFriends <- subset(weddingGuests, is_family_friend == TRUE)
  preCollegeFriends <- subset(weddingGuests, is_pre_college_friend == TRUE)
  collegeFriends <- subset(weddingGuests, is_college_friend == TRUE)
  postCollegeFriends <- subset(weddingGuests, is_post_college_friend == TRUE)
  attendedRehearsalDinner <- subset(weddingGuests, attended_rehearsal_dinner == TRUE)
  attendedWelcomeParty <- subset(weddingGuests, attended_welcome_party == TRUE)
  # fix this later and correct the data as well ("heard_from" instead of "heardFrom") so that it's more uniform.
  heardFrom <- subset(weddingGuests, heardFrom == TRUE)
  # isOfficiant <- subset(weddingGuests, is_officiant == TRUE)
  isVendor <- subset(weddingGuests, is_vendor == TRUE | is_officiant == TRUE)
  
  
  if ("everyone" %in% input$guestTypeSelect) {
    guests_to_be_plotted <- rbind(guests_to_be_plotted, everyone)
  }
  if ("is_couple" %in% input$guestTypeSelect) {
    guests_to_be_plotted <- rbind(guests_to_be_plotted, couple)
  }
  if ("invited" %in% input$guestTypeSelect) {
    guests_to_be_plotted <- rbind(guests_to_be_plotted, invited)
  }
  if ("attended_wedding" %in% input$guestTypeSelect) {
    guests_to_be_plotted <- rbind(guests_to_be_plotted, attendedWedding)
  }
  if ("brides_side" %in% input$guestTypeSelect) {
    guests_to_be_plotted <- rbind(guests_to_be_plotted, bridesSide)
  }
  if ("grooms_side" %in% input$guestTypeSelect) {
    guests_to_be_plotted <- rbind(guests_to_be_plotted, groomsSide)
  }
  if ("couples_side" %in% input$guestTypeSelect) {
    guests_to_be_plotted <- rbind(guests_to_be_plotted, couplesSide)
  }
  if ("wedding_party" %in% input$guestTypeSelect) {
    guests_to_be_plotted <- rbind(guests_to_be_plotted, weddingParty)
  }
  if ("brides_family" %in% input$guestTypeSelect) {
    guests_to_be_plotted <- rbind(guests_to_be_plotted, bridesFamily)
  }
  if ("grooms_family" %in% input$guestTypeSelect) {
    guests_to_be_plotted <- rbind(guests_to_be_plotted, groomsFamily)
  }
  if ("family" %in% input$guestTypeSelect) {
    guests_to_be_plotted <- rbind(guests_to_be_plotted, family)
  }
  if ("friends" %in% input$guestTypeSelect) {
    guests_to_be_plotted <- rbind(guests_to_be_plotted, friends)
  }
  if ("family_friends" %in% input$guestTypeSelect) {
    guests_to_be_plotted <- rbind(guests_to_be_plotted, familyFriends)
  }
  if ("pre_college_friends" %in% input$guestTypeSelect) {
    guests_to_be_plotted <- rbind(guests_to_be_plotted, preCollegeFriends)
  }
  if ("college_friends" %in% input$guestTypeSelect) {
    guests_to_be_plotted <- rbind(guests_to_be_plotted, collegeFriends)
  }
  if ("post_college_friends" %in% input$guestTypeSelect) {
    guests_to_be_plotted <- rbind(guests_to_be_plotted, postCollegeFriends)
  }
  if ("attended_rehearsal_dinner" %in% input$guestTypeSelect) {
    guests_to_be_plotted <- rbind(guests_to_be_plotted, attendedRehearsalDinner)
  }
  if ("attended_welcome_party" %in% input$guestTypeSelect) {
    guests_to_be_plotted <- rbind(guests_to_be_plotted, attendedWelcomeParty)
  }
  if ("heard_from" %in% input$guestTypeSelect) {
    guests_to_be_plotted <- rbind(guests_to_be_plotted, heardFrom)
  }
  if ("is_vendor" %in% input$guestTypeSelect) {
    guests_to_be_plotted <- rbind(guests_to_be_plotted, isVendor)
  }
  
  # https://stackoverflow.com/questions/13967063/remove-duplicated-rows
  noDuplicateGuests <- guests_to_be_plotted[!duplicated(guests_to_be_plotted), ]
  
  # end of new stuff
  print(noDuplicateGuests)
  return(noDuplicateGuests)
  
})





# weddingGuestsInputs(input)





weddingGuestsStateInputs <- reactive({
  
  weddingGuests <- weddingGuests
  
  # new stuff here
  
  # https://stackoverflow.com/questions/49851381/empty-a-data-frame-keep-colnames-headers-only
  states_to_be_plotted <- weddingGuests[FALSE, ]
  
  everyone <- weddingGuests
  couple <- subset(weddingGuests, is_bride == TRUE | is_groom == TRUE)
  invited <- subset(weddingGuests, invited == TRUE)
  attendedWedding <- subset(weddingGuests, attended_wedding == TRUE)
  bridesSide <- subset(weddingGuests, inviter == "Eyre")
  groomsSide <- subset(weddingGuests, inviter == "Germaine")
  couplesSide <- subset(weddingGuests, inviter == "Couple")
  weddingParty <- subset(weddingGuests, in_wedding_party == TRUE)
  bridesFamily <- subset(weddingGuests, is_family == TRUE & inviter == "Eyre")
  groomsFamily <- subset(weddingGuests, is_family == TRUE & inviter == "Germaine")
  family <- subset(weddingGuests, is_family == TRUE)
  friends <- subset(weddingGuests, is_friend == TRUE)
  familyFriends <- subset(weddingGuests, is_family_friend == TRUE)
  preCollegeFriends <- subset(weddingGuests, is_pre_college_friend == TRUE)
  collegeFriends <- subset(weddingGuests, is_college_friend == TRUE)
  postCollegeFriends <- subset(weddingGuests, is_post_college_friend == TRUE)
  attendedRehearsalDinner <- subset(weddingGuests, attended_rehearsal_dinner == TRUE)
  attendedWelcomeParty <- subset(weddingGuests, attended_welcome_party == TRUE)
  # fix this later and correct the data as well ("heard_from" instead of "heardFrom") so that it's more uniform.
  heardFrom <- subset(weddingGuests, heardFrom == TRUE)
  # isOfficiant <- subset(weddingGuests, is_officiant == TRUE)
  isVendor <- subset(weddingGuests, is_vendor == TRUE | is_officiant == TRUE)
  
  
  if ("everyone" %in% input$guestTypeSelect) {
    guests_to_be_plotted <- rbind(guests_to_be_plotted, everyone)
  }
  if ("is_couple" %in% input$guestTypeSelect) {
    guests_to_be_plotted <- rbind(guests_to_be_plotted, couple)
  }
  if ("invited" %in% input$guestTypeSelect) {
    guests_to_be_plotted <- rbind(guests_to_be_plotted, invited)
  }
  if ("attended_wedding" %in% input$guestTypeSelect) {
    guests_to_be_plotted <- rbind(guests_to_be_plotted, attendedWedding)
  }
  if ("brides_side" %in% input$guestTypeSelect) {
    guests_to_be_plotted <- rbind(guests_to_be_plotted, bridesSide)
  }
  if ("grooms_side" %in% input$guestTypeSelect) {
    guests_to_be_plotted <- rbind(guests_to_be_plotted, groomsSide)
  }
  if ("couples_side" %in% input$guestTypeSelect) {
    guests_to_be_plotted <- rbind(guests_to_be_plotted, couplesSide)
  }
  if ("wedding_party" %in% input$guestTypeSelect) {
    guests_to_be_plotted <- rbind(guests_to_be_plotted, weddingParty)
  }
  if ("brides_family" %in% input$guestTypeSelect) {
    guests_to_be_plotted <- rbind(guests_to_be_plotted, bridesFamily)
  }
  if ("grooms_family" %in% input$guestTypeSelect) {
    guests_to_be_plotted <- rbind(guests_to_be_plotted, groomsFamily)
  }
  if ("family" %in% input$guestTypeSelect) {
    guests_to_be_plotted <- rbind(guests_to_be_plotted, family)
  }
  if ("friends" %in% input$guestTypeSelect) {
    guests_to_be_plotted <- rbind(guests_to_be_plotted, friends)
  }
  if ("family_friends" %in% input$guestTypeSelect) {
    guests_to_be_plotted <- rbind(guests_to_be_plotted, familyFriends)
  }
  if ("pre_college_friends" %in% input$guestTypeSelect) {
    guests_to_be_plotted <- rbind(guests_to_be_plotted, preCollegeFriends)
  }
  if ("college_friends" %in% input$guestTypeSelect) {
    guests_to_be_plotted <- rbind(guests_to_be_plotted, collegeFriends)
  }
  if ("post_college_friends" %in% input$guestTypeSelect) {
    guests_to_be_plotted <- rbind(guests_to_be_plotted, postCollegeFriends)
  }
  if ("attended_rehearsal_dinner" %in% input$guestTypeSelect) {
    guests_to_be_plotted <- rbind(guests_to_be_plotted, attendedRehearsalDinner)
  }
  if ("attended_welcome_party" %in% input$guestTypeSelect) {
    guests_to_be_plotted <- rbind(guests_to_be_plotted, attendedWelcomeParty)
  }
  if ("heard_from" %in% input$guestTypeSelect) {
    guests_to_be_plotted <- rbind(guests_to_be_plotted, heardFrom)
  }
  if ("is_vendor" %in% input$guestTypeSelect) {
    guests_to_be_plotted <- rbind(guests_to_be_plotted, isVendor)
  }
  
  # https://stackoverflow.com/questions/13967063/remove-duplicated-rows
  noDuplicateGuests <- guests_to_be_plotted[!duplicated(guests_to_be_plotted), ]
  
  # end of new stuff
  
  return(noDuplicateGuests)
  
})






# "Everyone" = "everyone",
# "Couple" = "couple",
# "Invitees" = "",
# "Attendees",
# "Bride's Side",
# "Groom's Side",
# "Couple's Side",
# "Wedding Party",
# "Bride's Family",
# "Groom's Family",
# "Family",
# "Friends",
# "Family Friends",
# "Pre-College Friends",
# "College Friends",
# "Post-College Friends",
# "Rehearsal Dinner Attendees",
# "Welcome Party Attendees",
# "Vendors",
# "Heard From (but not invited)"


# isFamily <- subset(weddingGuests, is_family == TRUE)
# isFriend <- subset(weddingGuests, is_friend == TRUE)
# isFamilyFriend <- subset(weddingGuests, is_family_friend == TRUE)
# isPreCollegeFriend <- subset(weddingGuests, is_pre_college_friend == TRUE)
# isCollegeFriend <- subset(weddingGuests, is_college_friend == TRUE)
# isPostCollegeFriend <- subset(weddingGuests, is_post_college_friend == TRUE)
# attendedWelcomeParty <- subset(weddingGuests, attended_welcome_party == TRUE)
# attendedRehearsalDinner <- subset(weddingGuests, attended_rehearsal_dinner == TRUE)
# # fix this later and correct the data as well ("heard_from" instead of "heardFrom") so that it's more uniform.
# heardFrom <- subset(weddingGuests, heardFrom == TRUE)


# https://stackoverflow.com/questions/49851381/empty-a-data-frame-keep-colnames-headers-only
guests_to_be_plotted <- weddingGuests[FALSE, ]

everyone <- weddingGuests
couple <- subset(weddingGuests, is_bride == TRUE | is_groom == TRUE)
invited <- subset(weddingGuests, invited == TRUE)
attendedWedding <- subset(weddingGuests, attended_wedding == TRUE)
bridesSide <- subset(weddingGuests, inviter == "Eyre")
groomsSide <- subset(weddingGuests, inviter == "Germaine")
couplesSide <- subset(weddingGuests, inviter == "Couple")
weddingParty <- subset(weddingGuests, in_wedding_party == TRUE)
bridesFamily <- subset(weddingGuests, is_family == TRUE & inviter == "Eyre")
groomsFamily <- subset(weddingGuests, is_family == TRUE & inviter == "Germaine")
family <- subset(weddingGuests, is_family == TRUE)
friends <- subset(weddingGuests, is_friend == TRUE)
familyFriends <- subset(weddingGuests, is_family_friend == TRUE)
preCollegeFriends <- subset(weddingGuests, is_pre_college_friend == TRUE)
collegeFriends <- subset(weddingGuests, is_college_friend == TRUE)
postCollegeFriends <- subset(weddingGuests, is_post_college_friend == TRUE)
attendedRehearsalDinner <- subset(weddingGuests, attended_rehearsal_dinner == TRUE)
attendedWelcomeParty <- subset(weddingGuests, attended_welcome_party == TRUE)
# fix this later and correct the data as well ("heard_from" instead of "heardFrom") so that it's more uniform.
heardFrom <- subset(weddingGuests, heardFrom == TRUE)
# isOfficiant <- subset(weddingGuests, is_officiant == TRUE)
isVendor <- subset(weddingGuests, is_vendor == TRUE | is_officiant == TRUE)


if ("everyone" %in% input$type) {
  guests_to_be_plotted <- rbind(guests_to_be_plotted, everyone)
}
if ("is_couple" %in% input$type) {
  guests_to_be_plotted <- rbind(guests_to_be_plotted, couple)
}
if ("invited" %in% input$type) {
  guests_to_be_plotted <- rbind(guests_to_be_plotted, invited)
}
if ("attended_wedding" %in% input$type) {
  guests_to_be_plotted <- rbind(guests_to_be_plotted, attendedWedding)
}
if ("brides_side" %in% input$type) {
  guests_to_be_plotted <- rbind(guests_to_be_plotted, bridesSide)
}
if ("grooms_side" %in% input$type) {
  guests_to_be_plotted <- rbind(guests_to_be_plotted, groomsSide)
}
if ("couples_side" %in% input$type) {
  guests_to_be_plotted <- rbind(guests_to_be_plotted, couplesSide)
}
if ("wedding_party" %in% input$type) {
  guests_to_be_plotted <- rbind(guests_to_be_plotted, weddingParty)
}
if ("brides_family" %in% input$type) {
  guests_to_be_plotted <- rbind(guests_to_be_plotted, bridesFamily)
}
if ("grooms_family" %in% input$type) {
  guests_to_be_plotted <- rbind(guests_to_be_plotted, groomsFamily)
}
if ("family" %in% input$type) {
  guests_to_be_plotted <- rbind(guests_to_be_plotted, family)
}
if ("friends" %in% input$type) {
  guests_to_be_plotted <- rbind(guests_to_be_plotted, friends)
}
if ("family_friends" %in% input$type) {
  guests_to_be_plotted <- rbind(guests_to_be_plotted, familyFriends)
}
if ("pre_college_friends" %in% input$type) {
  guests_to_be_plotted <- rbind(guests_to_be_plotted, preCollegeFriends)
}
if ("college_friends" %in% input$type) {
  guests_to_be_plotted <- rbind(guests_to_be_plotted, collegeFriends)
}
if ("post_college_friends" %in% input$type) {
  guests_to_be_plotted <- rbind(guests_to_be_plotted, postCollegeFriends)
}
if ("attended_rehearsal_dinner" %in% input$type) {
  guests_to_be_plotted <- rbind(guests_to_be_plotted, attendedRehearsalDinner)
}
if ("attended_welcome_party" %in% input$type) {
  guests_to_be_plotted <- rbind(guests_to_be_plotted, attendedWelcomeParty)
}
if ("heard_from" %in% input$type) {
  guests_to_be_plotted <- rbind(guests_to_be_plotted, heardFrom)
}
if ("is_vendor" %in% input$type) {
  guests_to_be_plotted <- rbind(guests_to_be_plotted, isVendor)
}


noDuplicateGuests <- guests_to_be_plotted[!duplicated(guests_to_be_plotted), ]


# 
# if ("is_officiant" %in% input$type) {
#   guests_to_be_plotted <- rbind(guests_to_be_plotted, isOfficiant)
# }
# if ("in_wedding_party" %in% input$type) {
#   guests_to_be_plotted <- rbind(guests_to_be_plotted, inWeddingParty)
# }
# 
# if ("invited" %in% input$type) {
#   guests_to_be_plotted <- rbind(guests_to_be_plotted, invited)
# }
# if ("is_vendor" %in% input$type) {
#   guests_to_be_plotted <- rbind(guests_to_be_plotted, isVendor)
# }
# if ("is_family" %in% input$type) {
#   guests_to_be_plotted <- rbind(guests_to_be_plotted, isFamily)
# }
# if ("is_friend" %in% input$type) {
#   guests_to_be_plotted <- rbind(guests_to_be_plotted, isFriend)
# }
# if ("is_family_friend" %in% input$type) {
#   guests_to_be_plotted <- rbind(guests_to_be_plotted, isFamilyFriend)
# }
# if ("is_pre_college_friend" %in% input$type) {
#   guests_to_be_plotted <- rbind(guests_to_be_plotted, isPreCollegeFriend)
# }
# if ("is_college_friend" %in% input$type) {
#   guests_to_be_plotted <- rbind(guests_to_be_plotted, isCollegeFriend)
# }
# if ("is_post_college_friend" %in% input$type) {
#   guests_to_be_plotted <- rbind(guests_to_be_plotted, isPostCollegeFriend)
# }
# if ("attended_welcome_party" %in% input$type) {
#   guests_to_be_plotted <- rbind(guests_to_be_plotted, attendedWelcomeParty)
# }
# if ("attended_rehearsal_dinner" %in% input$type) {
#   guests_to_be_plotted <- rbind(guests_to_be_plotted, attendedRehearsalDinner)
# }
# if ("heard_from" %in% input$type) {
#   guests_to_be_plotted <- rbind(guests_to_be_plotted, heardFrom)
# }
# 
# 
# 
# # stuff <- subset(weddingGuests, input$type[1] == TRUE)
# 
# if ("attended_wedding" %in% input$type) {
#   print("yes")
# }print(stuff)
# 
# 
# if ("attended_wedding" %in% input$type) {
#   print("yes")
# }


# class(weddingGuests)
# 
# blank <- data.frame("name", "state", "popupContent", "generation", "groups", "geometry")
# 
# blank %>% add_row(weddingGuests[1])
# 
# print(weddingGuests[2])



# weddingGuests <- st_read("https://raw.githubusercontent.com/mgermaine93/wedding-guest-map/master/constants/guests.geojson")

# weddingGuests <- st_read("data/guests.geojson")
# 
# ui <- navbarPage(
#   
#   "Wedding Guest Map",
#   
#   mainPanel(
#     leafletOutput("leaflet")
#   )
#   
# )
# 
# server <- function(input, output) {
#   
#   output$leaflet <- renderLeaflet(
#     
#     map <- leaflet(data = weddingGuests) %>%
#       addTiles() %>%
#       setView(
#         lng = -98.583,
#         lat = 39.833,
#         zoom = 3
#       ) %>%
#       addMarkers(
#         weddingGuests$coords,
#         popup = ~as.character(popupContent),
#         clusterOptions = markerClusterOptions()
#       )
#     
#     # map %>% fitBounds(-72, 40, -70, 43)
#     
#   )
#   
# }
#   
# shinyApp(ui = ui, server = server)





# addMarkers(~long, ~lat, popup = ~as.character(mag), label = ~as.character(mag))

# data <- fromJSON("data/guests.geojson")
# weddingGuests <- cbind(data$features$properties, data$features$geometry)


# r_colors <- rgb(t(col2rgb(colors()) / 255))
# names(r_colors) <- colors()
# 
# ui <- fluidPage(
#   
#   # this is basically equivalent to "plotOutput" in the UI
#   leafletOutput("mymap"),
#   p(),
#   actionButton("recalc", "New points")
# )
# 
# server <- function(input, output, session) {
#   
#   points <- eventReactive(
#     # when the "recalc" button is clicked...
#     input$recalc, {
#     
#     cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
#   }, ignoreNULL = FALSE)
#   
#   # this is basically equivalent to "renderPlot" in the server
#   output$mymap <- renderLeaflet({
#     leaflet() %>%
#       addProviderTiles(
#         providers$Stamen.TonerLite,
#         options = providerTileOptions(
#           noWrap = TRUE
#           )
#       ) %>%
#       addMarkers(
#         data = points()
#       )
#   })
# }
# 
# shinyApp(ui, server)
