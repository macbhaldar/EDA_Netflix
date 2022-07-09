# Exploratory Data Analysis  of Netflix Dataset with Plotly in R

## Overview
This dataset consists of TV shows and Movies available on Netflix as of 2021. The dataset which can be found here: <https://www.kaggle.com/satpreetmakhija/netflix-movies-and-tv-shows-2021> is collected from a third-party Netflix search engine. 

## Data Reading

Lets read the data and rename it as "netfx" to get more useful and easy coding in functions. 

In the below we have to write `na.string=c("", "NA")` because, some values of our data are empty or taking place as `NA`. If we do not specify them at the begining in reading function, we can not reach the missing values in future steps.

```r
# read the dataset
netfx <- read.csv("netflixData.csv", na.strings = c("", "NA"), stringsAsFactors =FALSE) 
```

In the dataset there are 5960 observations of 17 following variables describing the tv shows and movies:

```r
# data table
library(plotly)

values_table <- rbind(c('Show_Id', 'Title', 'Description', 'Director', 'Genres', 'Cast', 'Production_Country', 'Release_Date', 'Rating', 'Duration', 'Imdb_Score' , 'Content_Type', 'Date_Added'),  c("Unique ID for every Movie / TV Show", 
     "Title of the Movie or TV Show", 
     "The summary description",
     "Director of the Movie /TV Show", 
     "Genere",
     "Actors involved in the Movie / TV Show",
     "Country where the movie / show was produced",
     "Release Date of the Movie or TV Show", 
     "Rating type of the Movie or TV Show",
     "Total Duration - in minutes or number of seasons",
     "IMDB Score of the Movie or TV Show",
     "Content type of the Movie or TV Show"))

fig_table <- plot_ly(
  type = 'table',
  columnorder = c(1,2),
  columnwidth = c(5,10),
  header = list(
    values = c('<b>VARIABLES</b><br>', '<b>DESCRIPTION</b>'),
    line = list(color = 'black'),
    fill = list(color = 'grey'),
    align = c('left','center'),
    font = list(color = 'white', size = 12),
    height = 40
  ),
  cells = list(
    values = values_table1,
    line = list(color = 'black'),
    fill = list(color = c('white', 'white')),
    align = c('left', 'left'),
    font = list(color = c('black'), size = 12),
    height = 30
  ))
  
fig_table
```

Output :

![plot_ly](./images/netflix_var.png)


## Data Cleaning

As a first step of the cleaning part, we can remove unnecessary variables and parts of the data such as `show_id` variable. Also description variable will not be used for the analysis or visualisation but it can be useful for the further analysis or interpretation.


```r
# data cleaning drop column
netfx$Show_Id <- NULL
```

`Rating` is categorical variable so we will change the type of it. 

```r
netds$rating <- as.factor(netfx$Rating)
```

We also can change the date format of `Date_Added` variable. 

```r
library(lubridate)
netfx$date_added <- mdy(netds$Date_Added)
```

`Content_Type` and `Genre` should be categorical variable

```r
netds$genres <- as.factor(netfx$Genres)
netds$type <- as.factor(netfx$Content_Type)
```

Missing values can be problem for the next steps. Therefore, we have to check them before the analyse and then we can fill the missing values of some variables if it is necessary. 

```r
# printing the missing values by creating a new data frame 
data.frame("Variable"=c(colnames(netfx)), "Missing Values"=sapply(netfx, function(x) sum(is.na(x))), row.names=NULL)
```

Output :

```
             Variable Missing.Values
1             Show_Id              0
2               Title              0
3         Description              0
4            Director           2064
5              Genres              0
6                Cast            530
7  Production_Country            559
8        Release_Date              3
9              Rating              4
10           Duration              3
11         Imdb_Score            608
12       Content_Type              0
13         Date_Added           1335
14             rating              4
15         date_added           1335
16             genres              0
17               type              0
```

We can clearly see that missing values take place in Director, Cast, Production_Country, Date_Added and rating variables. Since rating is the categorical variable with 17 levels we can fill in the missing values for rating with a mode.

```r
# function to find a mode
mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
netfx$rating[is.na(netfx$rating)] <- mode(netfx$rating)
```

Check again the filling

```r
data.frame("Variable"=c(colnames(netfx)), "Missing Values"=sapply(netfx, function(x) sum(is.na(x))), row.names=NULL)
```

Output :

```
             Variable Missing.Values
1               Title              0
2         Description              0
3            Director           2061
4              Genres              0
5                Cast            530
6  Production_Country            557
7        Release_Date              3
8              Rating              4
9            Duration              3
10         Imdb_Score            605
11       Content_Type              0
12         Date_Added           1335
13             rating              0
14         date_added           5960
15               type              0
16           new_date           1335
17             genres              0
```

Now, we are going to drop the missing values, at point where it will be necessary. We also drop duplicated rows in the dataset based on the "Title", "Production_Country", "Content_Type"," Release_Date" variables.

```r
#title, country, type and release_date
library(dplyr)
netfx=distinct(netfx, Title, Production_Country, Content_Type, Release_Date, .keep_all = TRUE)
```

## Data Visualuzation

### Amount of Netflix Content by Type

In the first graphy, ggplot2 library is used and data visualised with basic bar graph. In the code part, some arguments of functions will be described. 

```r
library(tibble)
library(dplyr)
library(ggplot2)
# craete a new table by the name of "amount_by_type" and applied some filter by using dplyr library. Primarly, group_by() function is used to select variable and then used summarise() function with n() to count number of TV Shows and Movies.
amount_by_type <- netfx %>% group_by(Content_Type) %>% summarise(
  count = n())
# In ggplot2 library, the code is created by two parts. First one is ggplot(), here we have to specify our arguments such as data, x and y axis and fill type. then continue with + and type of the graph will be added by using geom_graphytype.
figure00  <- ggplot(data = amount_by_type, aes(x= Content_Type, y= count, fill= type))+
  geom_bar(colour ="black", size= 0.2, fill = "blue" ,  stat = "identity")+
  guides(fill= FALSE)+
  xlab("Netflix Content by Type") + ylab("Amount of Netflix Content")+
  ggtitle("Amount of Netflix Content By Type")
ggplotly(figure00, dynamicTicks = T)
```

Output :

![plot_ly](./images/fig_type.png)

From above chart we concluded that there are more than nearly 2 times more Movies than TV Shows on Netflix.

### Amount of Netflix Content By Time

```r
# To see number contents by time we have to create a new data.frame.
f <- netfx$Title
f <-tibble(f)
netfx$Title <- f

# new_date variable created by selecting just years.
library(lubridate)
netfx$new_date <- year(netfx$date_added)

# df_by_date crated as a new grouped data frame. Titles are grouped depending the new_date(year) and then na.omit function applied to date column to remove NA values. Finally, number of added contents in a day calculated by using summarise() and n() functions.
df_by_date <- netfx$Title %>%
  group_by(netfx$new_date, netfx$type) %>%
  na.omit(netfx$new_date) %>%
  summarise(added_content_num = n())

# visualize new grouped data frame.
library(ggplot2)

Type<- df_by_date$`netfx$type`
Date <- df_by_date$`netfx$new_date`
Content_Number <- df_by_date$added_content_num
df_grouped<- ggplot(df_by_date, aes(Date, Content_Number))+
  geom_line(aes(colour = Type), size = 2)+
  geom_point() +
  xlab("Date") +
  ylab("Number of Content")+
  ggtitle("Amount of Netflix Content By Time")
ggplotly(df_grouped, dynamicTicks = T)
```
Output :

![plot_ly](./images/fig_time.png)

We see that starting from the year 2015 the total amount of content was growing exponentially. We also notice how fast the amount of movies on Netflix overcame the amount of TV Shows. The reason for the decline in 2021 is that the data we have is ending begining of the 2021.

### Amount of Netflix Content By Top 10 Country

```r
# split the countries in the country column by using strsplit() function and 
# then assign this operation to "k" for future use.
k <- strsplit(netfx$Production_Country, split = ", ")

# craete a new dataframe by using data.frame() function. First column should be type = second one country=. Created type column by using rep() function. 
# The function replicates the values in netfx$type depends on the length of each element of k. we used sapply()) function. 
# Now k is our new data in sapply(). it means that calculate the lenght of each element of the k list so that we create type column. 
# In the country column, we used just unlist() function. 
# It simply converts the list to vector with all the atomic components are being preserved.
netfx_countries<- data.frame(type = rep(netfx$Content_Type, sapply(k, length)), Production_Country = unlist(k))

# Change the elements of country column as character by using as.charachter() function.
netfx_countries$Production_Country <- as.character(netfx_countries$Production_Country)

# craete new grouped data frame by the name of amount_by_country. NA.omit() function deletes the NA values on the country column/variable. 
# Then we groupped countries and types by using group_by() function. After that used summarise() function to summarise the counted number of observations on the new "count" column by using n() function.
amount_by_country <- na.omit(netfx_countries) %>%
  group_by(Production_Country, type) %>%
  summarise(count = n())

# Actually we can use the "amount_by_country" dataframe to observe number of TV Show or Movie in countries. However, this list is too big to be visualized. Thus, we will create a new dataframe as table to see just top 10 countries by the name of "u".
# reshape() function will be used to create a reshaped grouped data. amount_by_country is used as data in the function. In this function, we will describe id variable, names of the value, time variable, and direction. Direction is character string, partially matched to either "wide" to reshape to wide format, or "long" to reshape to long format. Then we applied arrange() funtion to the reshaped grouped data. The dplyr function arrange() can be used to reorder (or sort) rows by one or more variables. In this part we sort count.movie column as descending.
# To check to arguments and detailed descriptions of functions please use to help menu or google.com
# After the arrange funtion, top_n() function is used to list the specified number of rows.
u <- reshape(data=data.frame(amount_by_country),idvar="Production_Country",
             v.names = "count",
             timevar = "type",
             direction="wide") %>% arrange(desc(count.Movie)) %>%
  top_n(10)

# names of the second and third columns are changed by using names() function as seen below.
names(u)[2] <- "Number_of_Movies"
names(u)[3] <- "Number_of_TV_Shows"

# In the arrange() function we sorted our count.movie columns as descending but, now, we want to change this sort depends on the total values of "number of Movies" and "number of TV Shows". To sort a data frame in R, use the order() function. By default, sorting is ASCENDING. Therefore, we have to specify as descending. + is used to specify total operation.
u <- u[order(desc(u$Number_of_Movies +u$Number_of_TV_Shows)),]

# create our graph by using ggplot2 library. First argument of the ggplot function is our data.frame, then we specified our variables in the aes() function. coloured the graphy depends on the countries. Then typeof the graph is writed as geom_point and dot size specified as 5. After that we named x and y axis. Title of the graph is wroted by using ggtitle() function.

library(ggplot2)

fig_country <- ggplot(u, aes(Number_of_Movies, Number_of_TV_Shows, colour=Production_Country))+
  geom_point(size=5)+
  xlab("Number of Movies") + ylab("Number of TV Shows")+
  ggtitle("Amount of Netflix Content By Top 10 Country")
ggplotly(fig_country, dynamicTicks = T)
```

Output :

![plot_ly](./images/fig_country.png)

We see that the United States is a clear leader in the amount of content on Netflix. 

### Amount of Content by Rating

```r
library(plotly)
data <-netfx$Title %>%
  group_by(netfx$Rating) %>%
  summarise(content_num = n())
names(data) [1] <- "rating"
names(data) [2] <- "content"
# From the above, we created our new table to use in graph
fig_rat <- plot_ly(data, labels = ~rating, values = ~content, type = 'pie')
# In the first part of visualisation, again, we have to specify our data labels, values,  x ad y axis and type of graph.
# In second part, adding title and other arguments of graph.
fig_rat <- fig_rat %>% layout(title = 'Amount of Content by Rating',
                              xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                              yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
fig_rat
```

Output :

![plot_ly](./images/fig_rat.png)

### Amount of content by Rating (Movie vs. TV Show)

```r
data2 <-netfx$Title %>%
  group_by(netfx$rating, netfx$type)%>%
  summarise(content_num = n())
names(data2) [1] <- "rating"
names(data2) [2] <- "type"
names(data2) [3] <- "content"
newdata2 <- reshape(data=data.frame(data2),idvar="rating",
                    v.names = "content",
                    timevar = "type",
                    direction="wide")


names(newdata2)[2] <- "Movie"
names(newdata2)[3] <- "TV Show"
#
newdata2$`TV Show`[is.na(newdata2$`TV Show`)] <- print(0)
# visualisation
library(plotly)
rating <- newdata2$rating
Movie <- newdata2$Movie
Tv_Show <- newdata2$`TV Show`
fig_rat2 <- plot_ly(newdata2, x = ~rating, y = ~Movie, type = 'bar', name = 'Movie')
fig_rat2 <- fig_rat2 %>% add_trace(y = ~Tv_Show, name = 'TV Show')
fig_rat2 <- fig_rat2 %>% layout(yaxis = list(title = 'Count'),
                              barmode = 'stack',
                              title="Amount of Content By Rating (Movie vs. TV Show)")
fig_rat2
```

Output :

![plot_ly](./images/fig_rat2.png)

### Top 20 Genres on NETFLIX

```r
library(crayon)
# before apply to strsplit function, we have to make sure that type of the variable is character.
netfx$genres<- as.character(netfx$genres)
t20 <- strsplit(netfx$genres, split = ", ")
count_genres<- data.frame(type = rep(netfx$type,
                                     sapply(t20, length)),
                          genres = unlist(t20))
count_genres$genres <- as.character(gsub(",","",count_genres$genres))
df_count_genres <- count_genres %>%
  group_by(genres) %>%
  summarise(count = n()) %>%
  top_n(20)
# visualisation
fig_gen <- plot_ly(df_count_genres, x= ~genres, y= ~df_count_genres$count, type = "bar" )
fig_gen <- fig_gen %>% layout(xaxis=list(categoryorder = "array",
                                         categoryarray = df_count_genres$genres,
                                         title="Genre"), yaxis = list(title = 'Count'),
                              title="20 Top Genres On Netflix")
fig_gen
```

Output :

![plot_ly](./images/fig_gen.png)


### Top 10 Directors By The Amount of Content on Netflix

```r
dir10 <- strsplit(netfx$Director, split = ", ")
titles_director <-  data.frame(type= rep(netfx$type, sapply(dir10, length)), director = unlist(dir10))
titles_director$director <- as.character(gsub(","," ", titles_director$director))
titles_director <- na.omit(titles_director) %>%
  group_by(director) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  top_n(10)
titles_director <- as.data.frame(titles_director)
library(tibble)
titles_director<- titles_director %>%
  remove_rownames %>%
  column_to_rownames(var = "director")
# visualisation as table
fig_dir <- plot_ly(
  type = 'table',
  header = list(
    values = c( '<b>Director<b>', names(titles_director)),
    align = c('left', rep('center', ncol(titles_director))),
    line = list(width = 1, color = 'black'),
    fill = list(color = 'grey'),
    font = list(family = "Arial", size = 15, color = "white")
  ),
  cells = list(
    values = rbind(
      rownames(titles_director),
      t(as.matrix(unname(titles_director)))
    ),
    align = c('left', rep('center', ncol(titles_director))),
    line = list(color = "black", width = 1),
    fill = list(color = c('white')),
    font = list(family = "Arial", size = 12, color = c("black"))
  ))
fig_dir
```

Output :

![plot_ly](./images/fig_dir.png)

