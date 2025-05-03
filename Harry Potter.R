chapters <- read.csv("Chapters.csv")
characters <- read.csv("Characters.csv")
dialogue <- read.csv("Dialogue.csv")
movies <- read.csv("Movies.csv")
places <- read.csv("Places.csv")
spells <- read.csv("Spells.csv")

library(tidyr)
library(ggplot2)
library(dplyr)
library(stringr)

 ##### 1. Most lines of dialogue #####
# join data 
chapters_dialogue <- dialogue %>%
  inner_join(chapters,by=c("Chapter.ID" = "Chapter.ID"))


# View(chapters_dialogue)

# To count the most number of dialogue
chapters_dialogue <- chapters_dialogue %>%
  group_by(Movie.ID) %>%
  summarize(max_lines = n())

##### 2. Most lines = longest runtime? #####
chapters_dialogue_movies <-chapters_dialogue %>%
  inner_join(movies,by=c("Movie.ID"="Movie.ID"))

# View(chapters_dialogue_movies)

chapters_dialogue_movies %>%
distinct(Runtime,Movie.Title,max_lines) %>%
  arrange(desc(Runtime))

##### 3. Average runtime #####
round(mean(chapters_dialogue_movies$Runtime),0) # Average runtime

chapters_dialogue_movies %>%
     filter(Runtime>=147) # TO find films that are above or equal average runtime

##### 4. Most-used spells #####
incantation <- spells$Incantation
class(incantation)
# View(incantation)

incantation_list <- as.vector(incantation)
# incantation_list
# class(incantation_list)

incantation_count <- list() # Create a list to store the FOR loop output

# FOR loop
for (i in incantation_list) {
 output<- print (i)
output <- print(length(grep((i),chapters_dialogue_movies$Dialogue)))
  incantation_count[i] <- output
}

# View(incantation_count)
chapters_dialogue_movies%>%
  group_by()

# accio
length(grep("Accio",chapters_dialogue_movies$Dialogue))

 
# str_detect(chapters_dialogue_movies$Dialogue,"Accio")

##### 5. Most popular location #####
chapters_dialogue_movies_places <- chapters_dialogue_movies %>%
  inner_join(places,by=c("Place.ID"="Place.ID"))
 
# Most-seen location in general
chapters_dialogue_movies_places %>%
      group_by(Place.ID,Place.Name)%>%
      summarize(occurence = n())%>%
      arrange(desc(occurence))%>%
     print(n=100)

# View(chapters_dialogue_movies_places)

# Most-seen location by film
 chapters_dialogue_movies_places %>%
   group_by(Place.ID,Place.Name, Movie.Title)%>% 
   summarize(occurence = n())%>%
   arrange(desc(occurence))%>%
   print(n=100)
 
 
 ##### 6. Dialogue by characters by movie #####
 chapters_dialogue_movies_places_characters <- chapters_dialogue_movies_places %>%
   inner_join(characters,by=c("Character.ID"="Character.ID"))
  
 # View(chapters_dialogue_movies_places_characters)
 
 # The character with the most lines ever
 chapters_dialogue_movies_places_characters %>%
   group_by(Character.Name) %>%
   summarize(dialogue_count = n()) %>%
   top_n(15) %>%
   arrange(desc(dialogue_count))
 
 
 # Characters with the most lines by movie
 chapters_dialogue_movies_places_characters %>%
   group_by(Character.Name,Movie.Title) %>%
   summarize(dialogue_count = n()) %>%
   top_n(1) %>%
   arrange(desc(dialogue_count))
 
# Plot to see lines by character by movies
 ggplot(data=chapters_dialogue_movies_places_characters,
        aes(x=chapters_dialogue_movies_places_characters$Character.Name,
            fill=chapters_dialogue_movies_places_characters$Movie.Title)) +
   geom_bar(position="dodge") +
   # scale_color_manual(values=wes_palette(n=5,name="GrandBudapest")) +
   scale_x_discrete(limits=c("Harry Potter", "Ron Weasley","Hermione Granger",
                             "Albus Dumbledore","Rubeus Hagrid")) +
   theme(legend.position = "bottom",
         legend.title = element_blank(),  # remove legend title
         panel.grid = element_blank(),
         legend.key.size = unit(.2, 'cm')) + # change legend font size
   xlab("Characters") + # Change x-axis title 
    ggtitle("Dialogue by film titles (Top five characters only)")

 # # These are the top 15
 # "Harry Potter", "Ron Weasley","Hermione Granger",
 # "Albus Dumbledore","Rubeus Hagrid","Severus Snape",
 # "Minerva McGonagall", "Horace Slughorn", "Voldemort",
 # "Neville Longbottom", "Remus Lupin", "Draco Malfoy",
 # "Alastor Moody", "Fred Weasley", "Arthur Weasley",
 # "Dolores Umbridge" 

 
 ##### 7. Box office performance #####
 movies$Budget <- gsub("\\$", "", movies$Budget) # Remove ($) sign
 movies$Budget <- gsub("\\,", "", movies$Budget) # Remove ($) sign
 movies$Box.Office <- gsub("\\$", "", movies$Box.Office) # Remove ($) sign
 movies$Box.Office <- gsub("\\,", "", movies$Box.Office) # Remove ($) sign
 
 movies$Budget <-as.numeric(movies$Budget) # convert to numeric type
 class(movies$Budget)
 
 movies$Box.Office <-as.numeric(movies$Box.Office) # convert to numeric type
 movies$Profits <- movies$Box.Office - movies$Budget # create 'Profits' column
 
 movies$Runtime <- as.numeric(movies$Runtime)
 movies$Release.Year <- as.numeric(movies$Release.Year)
 
 cor.test(movies$Profits,movies$Runtime) # Correlation
 #regex_cheat
 
 
 runtime_profits_model <-lm(movies$Profits ~ movies$Runtime,data=movies) # linear reg
 summary(runtime_profits_model)
 
 runtime_profits_model2 <-lm(movies$Profits ~ movies$Runtime + movies$Release.Year,data=movies)
 summary(runtime_profits_model2)
 
 cor.test(movies$Release.Year,movies$Profits) # Correlation
 
 ##### 8. Spell colour light #####
 spells %>%
   group_by(spells$Light) %>%
   summarise(count =n()) %>%
   arrange(desc(count))
 
 spells%>%
   filter(spells$Light=="Red")
 
  
