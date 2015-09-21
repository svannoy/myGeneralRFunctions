
# Create sample data frame with 3 values, the first of which is to be dropped for the subgrouping we want
sampleDfTbl <- tbl_df(data.frame(dummyVal = rep(1, 9), category = factor(rbinom(9,1, prob=0.5), labels=c("yes", "no")), value=rnorm(9)))

# If I just want the value column, it works
select(sampleDfTbl, value)

# Get the sub grouping
valueDf <- dplyr::group_by(sampleDfTbl, category) %>% dplyr::select(value)

# Now I want to extract 'values' for a specific level of category, but it keeps the category
filter(valueDf, category == "no") %>% select(value)

# Shouldn't his drop the category column?
select(valueDf, value)

# Hadley says you have to ungroup the data, here we see that it has some sort of group attribute
groups(valueDf)

# This should get rid of it
valueDf <- ungroup(valueDf)

# It does
groups(valueDf)

# And now it is gone
select(valueDf, value)


# Of course I can access it this way, but I want a 1 column data frame
valueDf$value

