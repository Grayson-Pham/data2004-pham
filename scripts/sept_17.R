library(tidyverse)

crashes <- read_csv("data/raw/crashes.csv")
persons <- read_csv("data/raw/person.csv")

glimpse(crashes)
glimpse(persons)

problems(crashes)

# What does one row represent in each table?
#In the crashes table one row represents 1 specific vehicle crash that happened in New York City
#In the persons table one row represents a recorded person that was involved with the crashes
# What variable appears in both? Does it do the same job in both?
#Collision ID, it is a unique ID that is used to identify a crash
crashes_core <- crashes |> 
  select(
    COLLISION_ID,
    `CRASH DATE`,
    BOROUGH,
    `NUMBER OF PERSONS INJURED`,
    `NUMBER OF PERSONS KILLED`
  )

persons_core <- persons |> 
  select(
    UNIQUE_ID,
    COLLISION_ID,
    PERSON_TYPE,
    PERSON_INJURY,
    PERSON_AGE,
    PERSON_SEX
  )

# You work for the NYC Department of Transportation and you have been given a media request: 
# How do person-level injury outcomes compare across boroughs? 

# Can you answer this with one dataframe alone? Probably not, or else we wouldn't do this on 
# our join day :)

# So we have to put these together. To do this, we have to work with keys. 

# A PRIMARY KEY uniquely identifies a row in its own table.
# A FOREIGN KEY points at another table's primary key.

# How should we figure out which one COLLISION_ID is?

##It tells us that in the documentation, or primary keys should be 1 to 1 with how many rows there are

crashes_core |> 
  count(COLLISION_ID) |> 
  filter(n > 1)

persons_core |> 
  count(UNIQUE_ID) |> 
  filter(n > 1)

persons_core |> 
  count(COLLISION_ID) |> 
  filter(n > 1) |> 
  arrange(desc(n))


# Why does COLLISION_ID repeat in the person table?
##Some crashes have more than 1 person involved
# Could we try to learn something about that wreck with 52 people in it? Let's use the persons df

# Do we have any missing keys?
crashes_core |> 
  filter(is.na(COLLISION_ID))

persons_core |> 
  filter(is.na(COLLISION_ID))

persons_core |> 
  filter(is.na(UNIQUE_ID))


# Cardinality

# Cardinality is how many rows on each side can share a key value. 
## one-to-one: each key appears once in both
## one-to-many: unique on the left, repeats on the right
## many-to-many: repeats on both sides

# Which one do we have? What does that tell you about what a join will
# do to our rows?

##We will be using one to many because crashes can involve multiple people 

nrow(crashes_core)
nrow(persons_core)
n_distinct(persons_core$COLLISION_ID)

# Two of those numbers are close but not equal. What does that difference
# tell you before we join anything?
##this tells me that some rows will not get any people attached to it
# Let's do our join. What should we join by? What's your guess for the number of rows? 

# We'll start by doing a join that keeps observations where both cases exist. 
# Can anyone remember which join this is? 

# Use the console if you don't remember. 
##inner- joins matches but drops everything else
##left- joins matches but does not drop everything on the first dataset.


crashes_inner<- crashes_core |> 
  inner_join(persons_core, join_by(COLLISION_ID))

persons_inner<- persons_core |> 
  inner_join(crashes_core, join_by(COLLISION_ID))


# What does one row represent now? Is that the same as before?
## it is not the same, one row now represents the crash information and which added persons information to it

# Now the other one. inner_join keeps rows that matched in BOTH tables.
# Which one keeps every row of the LEFT table whether it matched or not.
crashes_left<- crashes_core |> 
  left_join(persons_core, join_by(COLLISION_ID))

glimpse(crashes_left)

nrow(crashes_inner) #317941
nrow(crashes_left) #318319

## we know that there are some records that do not have person records involved
# Where did the difference come from?
## the inner join dropped some things

# What is one row in crash_people_inner? What is one row in
# crash_people_left? Are they the same question?


sum(!is.na(crash_people_left$UNIQUE_ID))

# If someone asked how many people were involved in crashes, which of
# those two numbers would you hand them?

# When would you want inner_join? When would you want left_join?

##It depends on the comparison
##which depends on the question that you are asking/ being asked

# We said this was one-to-many. We can say that in the code and make R
# check it for us.

crashes_core |> 
  inner_join(persons_core, join_by(COLLISION_ID),
             relationship = "one-to-many")

crashes_core |> 
  inner_join(persons_core, join_by(COLLISION_ID),
             relationship = "one-to-one")

# What happened on the second one? Why would you want that?

# COVERAGE is which rows on each side found a match. The table got
# bigger, so nothing was lost, right?

crashes_core |> #would give you crashes without peope 
  anti_join(persons_core, join_by(COLLISION_ID)) |> 
  nrow()

persons_core |> #people without crashes (none)
  anti_join(crashes_core, join_by(COLLISION_ID)) |> 
  nrow()


# anti_join keeps left rows with NO match and adds no columns. Does the
# order matter here? Are those two lines asking the same question?

# What kind of crash has no person records?

# We can make R shout about this one too.

persons_core |> 
  left_join(crashes_core, join_by(COLLISION_ID), unmatched = "error")

# What does that argument buy you?

# The four mutating joins:
# inner_join = rows that matched in both
# left_join = every row in the LEFT table, matched or not
# right_join = every row in the right table
# full_join = everything from both

# Given all that, which one do you want for person-level outcomes with
# borough attached? Which table goes on the left?

##### Your turn #####

# Build a person-level table that includes borough.

# Before you write anything:
## What should one row represent when you're done?
## Which table goes on the left?
###one row should represent a person's information and then the borough that the crash occurred in 
###the persons table should go on the left


# Then:
## Join them.
## Declare the cardinality with relationship = and see if R agrees.
## Use anti_join() to look at whatever didn't match.
## Count records by BOROUGH, PERSON_TYPE, and PERSON_INJURY.

persons_borough<- persons_core |> 
  inner_join(crashes_core, join_by(COLLISION_ID), relationship = "many-to-one")

persons_core |> 
  anti_join(crashes_core, join_by(COLLISION_ID))

persons_borough |> 


# Some of those rows will have no borough. Before you filter them out:
# how many are there, and are they all missing for the same reason?
