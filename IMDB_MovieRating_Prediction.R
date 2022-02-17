movies = read.csv("films_fall_2021_training_data.csv")
attach(movies)

# What are the distributions of the variables? (Use Box Plots/Histograms)

### 1 - visualizing the data using boxplots and histograms
#imdb_score, included as figure 2 in the report
summary(imdb_score)
boxplot(imdb_score, col = 'green')
hist(x = imdb_score, breaks = 50, col ='green',
     main = 'Figure 2: Distribution of imdb score',
     xlab = 'imdb score',
     ylab = 'number of movies')
#budget_in_millions
summary(budget_in_millions)
boxplot(budget_in_millions, col = 'green')
hist(x = budget_in_millions, breaks = 50, col ='green')
#month_of_release
summary(month_of_release)
boxplot(month_of_release, col = 'orange')
hist(x = month_of_release, breaks = 50, col ='orange')
#year_of_release
summary(year_of_release)
boxplot(year_of_release, col = 'red')
hist(x = year_of_release, breaks = 50, col ='red')
#duration_in_hours
summary(duration_in_hours)
boxplot(duration_in_hours, col = 'pink')
hist(x = duration_in_hours, breaks = 50, col ='pink')
# total_number_of_actors
boxplot(total_number_of_actors, col = 'green')
hist(x = total_number_of_actors, breaks = 50, col ='green')
# total_number_of_directors, included as figure 1 in report
boxplot(total_number_of_directors, col = 'green')
hist(x = total_number_of_directors, breaks = 50, col ='green', 
     main = 'Figure 1: Distribution of the number of directors',
     xlab = 'number of directors',
     ylab = 'number of movies')
# total_number_of_producers
boxplot(total_number_of_producers, col = 'green')
hist(x = total_number_of_producers, breaks = 50, col ='green')
# total_number_of_production_companies
boxplot(total_number_of_production_companies, col = 'green')
hist(x = total_number_of_production_companies, breaks = 50, col ='green')
# total_number_of_production_countries
boxplot(total_number_of_production_countries, col = 'green')
hist(x = total_number_of_production_countries, breaks = 50, col ='green')
#main_production_country
summary(main_production_country)
library(ggplot2)
ggplot(movies, aes(x=reorder(main_production_country, main_production_country, function(x)-length(x)))) +
  geom_bar(fill='green') +  labs(x='Language') + ggtitle("Production Country") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#main_lang
summary(main_lang)
library(ggplot2)
ggplot(movies, aes(x=reorder(main_lang, main_lang, function(x)-length(x)))) +
  geom_bar(fill='red') +  labs(x='Language') + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#Pie charts for the binary categorical variables (genres) 
# We made/examined a pie chart for every binary variable, but only a selection are included in the code below to avoid being redundent.

#install.packages("lessR")
library(lessR)
#genre_action (0 or 1)
genre = data.frame(var = genre_action)
PieChart(var, hole = 0, values = "%", data = genre,
         fill = c("grey", "red"), main = "Pie Chart of genre_action")
legend("topleft", legend = c("Non-action", "Action"),
       fill =  c("grey", "red"))
#genre_animation (0 or 1)
genre = data.frame(var = genre_animation)
PieChart(var, hole = 0, values = "%", data = genre,
         fill = c("grey", "red"), main = "Pie Chart of genre_animation")
legend("topleft", legend = c("Non-animation", "animation"),
       fill =  c("grey", "red"))
#genre_adventure (0 or 1)
genre = data.frame(var = genre_adventure)
PieChart(var, hole = 0, values = "%", data = genre,
         fill = c("grey", "red"), main = "Pie Chart of genre_adventure")
legend("topleft", legend = c("Non-adventure", "adventure"),
       fill =  c("grey", "red"))
# the same code as above was used over and over for each movie genre variable...

#The pie chart generated from the code below was our first indication that there were no realitytv movies in the dataset.
# genre_realitytv (0 or 1)
genre = data.frame(var = genre_realitytv)
PieChart(var, hole = 0, values = "%", data = genre,
         fill = c("grey", "red"), main = "Pie Chart of genre_realitytv")
legend("topleft", legend = c("Non-realitytv", "realitytv"),
       fill =  c("grey", "red"))
#The pie chart generated from the code below was our first indication that there are no short movies in the dataset.
# genre_shortfilm (0 or 1)
genre = data.frame(var = genre_shortfilm)
PieChart(var, hole = 0, values = "%", data = genre,
         fill = c("grey", "red"), main = "Pie Chart of genre_shortfilm")
legend("topleft", legend = c("Non-shortfilm", "shortfilm"),
       fill =  c("grey", "red"))

#Below we create charts that visualize multiple independent variables at once. These are more interesting/informative charts for our report.

# Selecting subsets of Data to draw plots
df_numerical = subset(movies, select = c(budget_in_millions,
                                        year_of_release,	
                                        duration_in_hours,
                                        total_number_of_actors			
))

df_quantitative = subset(movies, select = c(month_of_release,
                                           total_number_languages,			
                                           total_number_of_directors,		
                                           total_number_of_producers,			
                                           total_number_of_production_companies,	
                                           total_number_of_production_countries))

numeric_categorical = c(budget_in_millions,
                        year_of_release,	
                        duration_in_hours,
                        total_number_of_actors,
                        month_of_release,
                        total_number_languages,			
                        total_number_of_directors,		
                        total_number_of_producers,			
                        total_number_of_production_companies,	
                        total_number_of_production_countries
)

## Charts to show distribution of numerical variables
# Included In Report as Appendix 3-1
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))

for (i in 1:ncol(df_numerical)) {
  
  hist(df_numerical[[i]], main = paste('Histogram of ', colnames(df_numerical)[i]), xlab = paste( colnames(df_numerical)[i]), breaks = 50, col = 'green')
}


## Charts to show distribution of quantitative variables
# Included In Report as Appendix 3-1
layout(matrix(c(1,2,3,4,5,6), 3, 2, byrow = TRUE))


for (i in 1:ncol(df_quantitative)) {
  
  hist(df_quantitative[[i]], main = paste('Histogram of ', colnames(df_quantitative)[i]), xlab = paste( colnames(df_quantitative)[i]), breaks = 50, col = 'green')
}

## Chart to show categorical genre variables
# Included In Report as Appendix 3-2
# Define Names and Values of barchart
df_genre = movies[, 11:34]
barplot_numeric_values = vector()
barplot_labels = colnames(df_genre)
for (i in 1:length(df_genre)){
  barplot_numeric_values[i] = sum(df_genre[i])
}

library(ggplot2)
library(dplyr)

# Create dataframe
data <- data.frame(
  name=barplot_labels,
  val=barplot_numeric_values
)

library(forcats)

# Reorder following the value of another column:
data %>%
  mutate(name = fct_reorder(name, val)) %>%
  ggplot( aes(x=name, y=val)) +
  geom_bar(stat="identity", fill="green", alpha=.6, width=.4) +
  coord_flip() +
  xlab("Categories") +
  ylab("Frequency")+
  theme_bw()


## Chart to show categorical is_female variables
# Included In Report as Appendix 3-2
# Define Names and Values of barchart
df_is_female = movies[, c(36, 38, 40,43)]
is_female_barplot_numeric_values = vector()
is_female_barplot_labels = colnames(df_is_female)
for (i in 1:length(df_is_female)){
  is_female_barplot_numeric_values[i] = sum(df_is_female[i])
}

# Create Dataframe
data <- data.frame(
  name=is_female_barplot_labels,
  val=is_female_barplot_numeric_values
)

# Reorder following the value of another column:
data %>%
  mutate(name = fct_reorder(name, val)) %>%
  ggplot( aes(x=name, y=val)) +
  geom_bar(stat="identity", fill="green", alpha=.6, width=.4) +
  coord_flip() +
  xlab("Categories") +
  ylab("Frequency")+
  theme_bw()

# Testing the skew of the variables
# Results included in the report as Appendix 4
#install.packages('moments')
library(moments) 

a = skewness(budget_in_millions,na.rm = TRUE)
#budget_in_millions right skewed
b = skewness(month_of_release,na.rm = TRUE)
#month_of_release left skewed
c=skewness(year_of_release,na.rm = TRUE)
#year_of_release left skewed
d=skewness(duration_in_hours,na.rm = TRUE)
#duration_in_hours right skewed
e=skewness(total_number_languages,na.rm = TRUE)
#total_number_languages right skewed
f=skewness(genre_action,na.rm = TRUE)
#genre_action right skewed
g=skewness(genre_adventure,na.rm = TRUE)
#right skewed
h=skewness(genre_animation,na.rm = TRUE)
#right skewed
i=skewness(genre_biography,na.rm = TRUE)
#right skewed
j=skewness(genre_comedy,na.rm = TRUE)
#right skewed
k=skewness(genre_crime,na.rm = TRUE)
#right skewed
l=skewness(genre_documentary,na.rm = TRUE)
#right skewed
m=skewness(genre_drama,na.rm = TRUE)
#right skewed
n=skewness(genre_family,na.rm = TRUE)
#right skewed
o=skewness(genre_fantasy,na.rm = TRUE)
#right skewed
p=skewness(genre_filmnoir,na.rm = TRUE)
#right skewed
q=skewness(genre_history,na.rm = TRUE)
#right skewed
r= skewness(genre_horror,na.rm = TRUE)
#right skewed
s=skewness(genre_history,na.rm = TRUE)
#right skewed
t=skewness(genre_music,na.rm = TRUE)
#right skewed
u=skewness(genre_musical,na.rm = TRUE)
#right skewed
v=skewness(genre_mystery,na.rm = TRUE)
#right skewed
w=skewness(genre_romance,na.rm = TRUE)
#right skewed
x=skewness(genre_scifi,na.rm = TRUE)
#right skewed
y=skewness(genre_sport,na.rm = TRUE)
#right skewed
z=skewness(genre_thriller,na.rm = TRUE)
#right skewed
aa=skewness(genre_war,na.rm = TRUE)
#right skewed
bb=skewness(genre_western,na.rm = TRUE)
#right skewed
cc=skewness(total_number_of_actors,na.rm = TRUE)
#total_number_of_actors right skewed
dd=skewness(total_number_of_producers,na.rm = TRUE)
#total_number_of_producers right skewed
ee=skewness(total_number_of_production_companies,na.rm = TRUE)
#total_number_of_production_companies right skewed
ff=skewness(total_number_of_production_countries,na.rm = TRUE)
#total_number_of_production_countries right skewed
##create a dataframe with the skewness of each predictor
predictors = c('budget_in_millions',
               'month_of_release',
               'year_of_release',
               'duration_in_hours',
               'total_number_languages',
               'genre_action',
               'genre_adventure',
               'genre_animation',
               'genre_biography',
               'genre_comedy',
               'genre_crime',
               'genre_documentary',
               'genre_drama',
               'genre_family',
               'genre_fantasy',
               'genre_filmnoir',
               'genre_history',
               'genre_horror',
               'genre_music',
               'genre_musical',
               'genre_mystery',
               'genre_romance',
               'genre_scifi',
               'genre_sport',
               'genre_thriller',
               'genre_war',
               'genre_western',
               'total_number_of_actors',
               'total_number_of_directors',
               'total_number_of_producers',
               'total_number_of_production_companies',
               'total_number_of_production_countries')
#create a vector out of the results of each skewness test
skewness = c(a,	b,	c,	d,	e,	f,	g,	h,	i,	j,	k,	l,	m,	n,	o,	p,	q,	r,	s,	t,	u,	v,	w,	x,	y,	z, aa,bb,cc,dd,ee,ff)
#create a table with the names of the predictors and their skewness
SKEW_Table = data.frame(predictors, skewness)

# Examine the correlation amongst the variables
#install.packages('reshape2')
library(reshape2)
library(ggplot2)
#create a dataframe with only the quantitative variables that are numeric class
movies1 = movies[,-c(11:34)]
movies2 = subset(movies1, select = -c(title, 
                                    imdb_id, 
                                    imdb_url, 
                                    imdb_score, 
                                    main_lang,
                                    main_actor1_name,
                                    main_actor2_name,
                                    main_actor3_name,
                                    main_director_name,
                                    main_producer_name,
                                    editor_name,
                                    main_production_company,
                                    main_production_country))

#create Pearson, Kendall, and Spearman correlation matrices
corr_matrix_pearson = cor(movies2, method = c("pearson"), use = "complete.obs")	

corr_matrix_kendall = cor(movies2, method = c("kendall"), use = "complete.obs")

corr_matrix_spearman = cor(movies2, method = c("spearman"), use = "complete.obs")

#crete correlation heat maps for each correlation matrix
#pearson heat map
# Included in report as Appendix 5
melted_corr_matrix_pearson <- melt(corr_matrix_pearson)
ggplot(data = melted_corr_matrix_pearson, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +  ggtitle('Pearson Correlation Heat Map') + xlab("Independent Variables") + ylab("Independent Variables")+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#kendall heat map
melted_corr_matrix_kendall <- melt(corr_matrix_kendall)
ggplot(data = melted_corr_matrix_kendall, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#spearman heat map
melted_corr_matrix_spearman <- melt(corr_matrix_spearman)
ggplot(data = melted_corr_matrix_spearman, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Check for outliers

#install.packages("lmtest")
#install.packages("plm")
require(lmtest)
require(plm)
library(car)
# identify the outliers in 2 ways
# Method 1
mreg =lm(imdb_score~
           budget_in_millions+
           month_of_release+
           year_of_release+
           duration_in_hours+
           total_number_languages+
           genre_action+
           genre_adventure+
           genre_animation+
           genre_biography+
           genre_comedy+
           genre_crime+
           genre_documentary+
           genre_drama+
           genre_family+
           genre_fantasy+
           genre_filmnoir+
           genre_history+
           genre_horror+
           genre_music+
           genre_musical+
           genre_mystery+
           genre_romance+
           genre_scifi+
           genre_sport+
           genre_thriller+
           genre_war+
           genre_western+
           total_number_of_actors+
           total_number_of_directors+
           total_number_of_producers+
           total_number_of_production_companies+
           total_number_of_production_countries)
qqPlot(mreg)

# Method 2
#run bonferroni test
outlierTest(mreg)
# remove the outliers (obs 633, 895, 2045, 2310, 2718, 526)
movies2=movies[-c(633, 895, 2045, 2310, 2718, 526), ]
# run regression w/o outliers
mreg_noout=lm(imdb_score~
                budget_in_millions+
                month_of_release+
                year_of_release+
                duration_in_hours+
                total_number_languages+
                genre_action+
                genre_adventure+
                genre_animation+
                genre_biography+
                genre_comedy+
                genre_crime+
                genre_documentary+
                genre_drama+
                genre_family+
                genre_fantasy+
                genre_filmnoir+
                genre_history+
                genre_horror+
                genre_music+
                genre_musical+
                genre_mystery+
                genre_romance+
                genre_scifi+
                genre_sport+
                genre_thriller+
                genre_war+
                genre_western+
                total_number_of_actors+
                total_number_of_directors+
                total_number_of_producers+
                total_number_of_production_companies+
                total_number_of_production_countries, data=movies2)

# Is there any collinearity among variables?
#VIF test
# Included in report as Appendix 5
#over 4 means collinearity 
vif(mreg_noout)
#using a VIF test, none of the the quantitative predictor variables are identified as being collinear.

#"""Summary of changes made to the original dataset based on the insights from the data description section of the report (above code)"""

#steps to prepare dataset for testing models:
#movies = read.csv("films_fall_2021_training_data.csv")
#attach(movies)

#Remove outliers from the dataset
movies=movies[-c(633, 895, 2045, 2310, 2718, 526), ]
#Remove columns that have 0 value for every observation
movies = subset(movies, select = -c(genre_shortfilm,genre_realitytv))
#Remove predictors that are unary
movies = subset(movies, select = -c(total_number_of_directors))
#Change main language to 1 or 0, 1 for English
movies$main_lang_English = ifelse(movies$main_lang == 'English', 1, 0)
movies = subset(movies, select = -c(main_lang))
#Change poduction country to 1 or 0, 1 for USA
movies$main_production_country_US = ifelse(movies$main_production_country == 'United States of America',1,0)
movies = subset(movies, select = -c(main_production_country))
#drop labels
movies = subset(movies, select = -c(title, imdb_id,imdb_url))
#set remaining character variables as factors
movies$main_actor1_name=as.factor(movies$main_actor1_name)
movies$main_actor2_name=as.factor(movies$main_actor2_name)
movies$main_actor3_name=as.factor(movies$main_actor3_name)
movies$main_director_name=as.factor(movies$main_director_name)
movies$main_producer_name=as.factor(movies$main_producer_name)
movies$editor_name=as.factor(movies$editor_name)
movies$main_production_company=as.factor(movies$main_production_company)

#omit NAs
movies <- na.omit(movies)
attach(movies)

#"""## Step 3
#Code updated (2021 Nov 12): 
#based on cleanmovies.csv (result from data description section above)
#"""

# Step 3

#attach(cleanmovies)

## for numeric variables
numeric = subset(movies, select = -c(main_actor1_name, main_actor2_name, main_actor3_name, main_director_name,main_producer_name,editor_name,main_production_company) )

attach(numeric)
library(car)
library(data.table)

## create a dataframe to store the result
step3_result = data.frame(matrix(NA, nrow = 36, ncol = 6))
setNames(step3_result, c("corr_coef", "coefficient of Xi", "p-value of coefficient", "r-squred","residual Pr-value","NCV p-value"))
setattr(step3_result, "names",  c("corr_coef","coefficient of Xi", "p-value of coefficient", "r-squared","residual Pr-value","NCV p-value"))

## run tests and store result into the dataframe
for (i in 2:37) {
  rownames(step3_result)[i-1] <- names(numeric[i])
  # correlation coefficient between Y and Xi
  step3_result[i-1,1] = cor(numeric[,i], imdb_score)
  
  # Run simple linear regressions between Y and the predictor
  slreg = lm(imdb_score~numeric[,i])
  step3_result[i-1,2]=slreg$coefficients[2]
  step3_result[i-1,3]=anova(slreg)$'Pr(>F)'[1]
  step3_result[i-1,4]=summary(slreg)$r.squared
  
  # run residual plot to check linearity
  step3_result[i-1,5]=residualPlots(slreg,plot='FALSE',tests='FALSE')[1,2]
  # If Pr-value < 0.05, This predictor is non-linear
  
  # non-constant variance (NCV) test for heteroskedasticity
  step3_result[i-1,6]=ncvTest(slreg)$p
  # P-value > 0.05, no heteroskedasticity
  
}

# #"""# **Part 4**"""
# #raw_movies = read.csv("films_fall_2021_training_data.csv")
# #attach(raw_movies)
# #df2 = subset(raw_movies, select = c(imdb_score,budget_in_millions,month_of_release,year_of_release,duration_in_hours,total_number_languages,total_number_of_actors,total_number_of_producers,total_number_of_production_companies,total_number_of_production_countries) )
# 
# #library(boot)
# #attach(df2)
# #library(car)
# library(data.table)
# step4_result_pol = data.frame(matrix(NA, nrow = 9, ncol = 3))
# setattr(step4_result_pol, "names",  c("Pol-deg","Pol-Min-MSE","Pol-r-squred"))
# 
# for (i in 2:10) {
#   rownames(step4_result_pol)[i-1] <- names(df2[i])
#   # Pol result
#   fit=glm(df2[,"imdb_score"]~df2[,i], data=df2)
#   mse=cv.glm(df2, fit, K=10)$delta[1]
#   step4_result_pol[i-1,2]=mse
#   step4_result_pol[i-1,1]=1
#   slreg = lm(imdb_score~df2[,i])
#   step4_result_pol[i-1,3]=summary(slreg)$r.squared
#   for (j in 2:5){
#     fit=glm(df2[,"imdb_score"]~poly(df2[,i],j), data=df2)
#     mse=cv.glm(df2, fit, K=10)$delta[1]
#     if (mse <step4_result_pol[i-1,2]) {
#       step4_result_pol[i-1,2]=mse
#       step4_result_pol[i-1,1]=j
#       reg = lm(imdb_score~poly(df2[,i],j), data=df2)
#       step4_result_pol[i-1,3]=summary(reg)$r.squared
#     }
#   }
# }
# 
# 
# attach(numeric)
# ## create a dataframe to store the result
# step4_result_spl = data.frame(matrix(NA, nrow = 36, ncol = 4))
# setattr(step4_result_spl, "names",  c("Spl-#k", "Spl-deg","Spl-Min-MSE","Spl-r-squred"))
# 
# #knots=0
# for (i in 3:38){
#   rownames(step4_result_spl)[i-2] <- names(numeric[i])
#   fit=glm(numeric[,"imdb_score"]~numeric[,i], data=numeric)
#   mse=cv.glm(numeric, fit, K=10)$delta[1]
#   step4_result_spl[i-2,3]=mse
#   step4_result_spl[i-2,2]=1
#   step4_result_spl[i-2,1]=0
#   slreg = lm(imdb_score~numeric[,i])
#   step4_result_spl[i-2,4]=summary(slreg)$r.squared
# }
# 
# #knots=1
# for (i in 3:38) {
#   k1=quantile(numeric[,i],1/2)
#   for (j in 1:5){
#     fit=glm(numeric[,"imdb_score"]~bs(numeric[,i],knots=c(k1), degree=j), data=numeric)
#     mse=cv.glm(numeric, fit, K=10)$delta[1]
#     if (mse <step4_result_spl[i-2,3]) {
#       step4_result_spl[i-2,3]=mse
#       step4_result_spl[i-2,2]=j
#       step4_result_spl[i-2,1]=1
#       spl=lm(imdb_score~bs(numeric[,i],knots=c(k1), degree=j))
#       step4_result_spl[i-2,4]=summary(spl)$r.squared
#     }
#   }
# }   
# 
# 
# #knots=2
# for (i in 3:38) {
#   k1=quantile(numeric[,i],1/3)
#   k2=quantile(numeric[,i],2/3)
#   for (j in 1:5){
#     fit=glm(numeric[,"imdb_score"]~bs(numeric[,i],knots=c(k1,k2), degree=j), data=numeric)
#     mse=cv.glm(numeric, fit, K=10)$delta[1]
#     if (mse <step4_result_spl[i-2,3]) {
#       step4_result_spl[i-2,3]=mse
#       step4_result_spl[i-2,2]=j
#       step4_result_spl[i-2,1]=2
#       spl=lm(imdb_score~bs(numeric[,i],knots=c(k1,k2), degree=j))
#       step4_result_spl[i-2,4]=summary(spl)$r.squared
#     }
#   }
# }   
# 
# #knots=3
# for (i in 3:38) {
#   k1=quantile(numeric[,i],1/4)
#   k2=quantile(numeric[,i],2/4)
#   k3=quantile(numeric[,i],3/4)
#   for (j in 1:5){
#     fit=glm(numeric[,"imdb_score"]~bs(numeric[,i],knots=c(k1,k2,k3), degree=j), data=numeric)
#     mse=cv.glm(numeric, fit, K=10)$delta[1]
#     if (mse <step4_result_spl[i-2,3]) {
#       step4_result_spl[i-2,3]=mse
#       step4_result_spl[i-2,2]=j
#       step4_result_spl[i-2,1]=3
#       spl=lm(imdb_score~bs(numeric[,i],knots=c(k1,k2,k3), degree=j))
#       step4_result_spl[i-2,4]=summary(spl)$r.squared
#     }
#   }
# }   
# 
# #knots=4
# for (i in 3:38) {
#   k1=quantile(numeric[,i],1/5)
#   k2=quantile(numeric[,i],2/5)
#   k3=quantile(numeric[,i],3/5)
#   k4=quantile(numeric[,i],4/5)
#   for (j in 1:5){
#     fit=glm(numeric[,"imdb_score"]~bs(numeric[,i],knots=c(k1,k2,k3,k4), degree=j), data=numeric)
#     mse=cv.glm(numeric, fit, K=10)$delta[1]
#     if (mse <step4_result_spl[i-2,3]) {
#       step4_result_spl[i-2,3]=mse
#       step4_result_spl[i-2,2]=j
#       step4_result_spl[i-2,1]=4
#       spl=lm(imdb_score~bs(numeric[,i],knots=c(k1,k2,k3,k4), degree=j))
#       step4_result_spl[i-2,4]=summary(spl)$r.squared
#     }
#   }
# }   
# 
# #knots=5
# for (i in 3:38) {
#   k1=quantile(numeric[,i],1/6)
#   k2=quantile(numeric[,i],2/6)
#   k3=quantile(numeric[,i],3/6)
#   k4=quantile(numeric[,i],4/6)
#   k5=quantile(numeric[,i],5/6)
#   for (j in 1:5){
#     fit=glm(numeric[,"imdb_score"]~bs(numeric[,i],knots=c(k1,k2,k3,k4,k5), degree=j), data=numeric)
#     mse=cv.glm(numeric, fit, K=10)$delta[1]
#     if (mse <step4_result_spl[i-2,3]) {
#       step4_result_spl[i-2,3]=mse
#       step4_result_spl[i-2,2]=j
#       step4_result_spl[i-2,1]=5
#       spl=lm(imdb_score~bs(numeric[,i],knots=c(k1,k2,k3,k4,k5), degree=j))
#       step4_result_spl[i-2,4]=summary(spl)$r.squared
#     }
#   }
# }

##Processing actor and director name variables so that they can be included in our models

# Setting Actor1 and Directors as Factor to be used in the model
movies$main_actor1_name = as.factor(movies$main_actor1_name)
movies$main_director_name = as.factor(movies$main_director_name)


# Frequency Table of Actors based on the number of movies acted in the dataset
a = as.data.frame(table(movies$main_actor1_name))
mean(a$Freq)
median(a$Freq)
b <- a[order(a$Freq, decreasing = TRUE), ]

# Frequency Table of Directors based on the number of movies directed in the dataset
c = as.data.frame(table(movies$main_director_name))
d <- c[order(c$Freq, decreasing = TRUE), ]
mean(c$Freq)
median(c$Freq)

# Adjust the levels to only include actors with more than 6 films and label all others as "Others"
levels(movies$main_actor1_name)[levels(movies$main_actor1_name) %in% b[80:nrow(b),1] ] <-"Others"

# Adjust the levels to only include directors with more than 6 films and label all others as "Others"
levels(movies$main_director_name)[levels(movies$main_director_name) %in% d[68:nrow(d),1] ] <-"Others"


# Relevel the lables to set "Others" as the base level
movies$main_actor1_name = relevel(movies$main_actor1_name, ref="Others")
movies$main_director_name = relevel(movies$main_director_name, ref="Others")
attach(movies)

# Run Regressions to determine directors and actors who significantly impact the rating (Lower P-value than <0.05)
a1reg= lm(movies$imdb_score ~ movies$main_director_name, data=movies)
summary(a1reg)
a2reg= lm(movies$imdb_score ~ movies$main_actor1_name, data=movies)
summary(a2reg)

# Finding out who are the signifcant actors based on their p value coefficient in regression
significants = c()
for (i in 2:length(summary(a2reg)$coefficients[,4])){
  
  if( summary(a2reg)$coefficients[,4][i]  <0.1){
    
    significants = c(significants, i)
  }
}
# Record the names of significant actors to be used in the final model
significant_actors1 = vector()

for (i in 1:length(significants)){
  significant_actors1 = c(significant_actors1,levels(movies$main_actor1_name)[significants[i]] )
}

## Finding out who are the signifcant directors based on their p value coefficient in regression
significants = c()
for (i in 2:length(summary(a1reg)$coefficients[,4])){
  
  if( summary(a1reg)$coefficients[,4][i]  < 0.1){
    
    significants = c(significants, i)
  }
}
# Record the names of significant directors to be used in the final model

significant_directors = vector()

for (i in 1:length(significants)){
  significant_directors = c(significant_directors,levels(movies$main_director_name)[significants[i]] )
}

# Define not in function 
'%notin%' <- Negate('%in%')


# Excluding all directors/actors who are not significantly impacting ratings and labeling them as others
levels(movies$main_actor1_name)[levels(movies$main_actor1_name) %notin% significant_actors1 ] <-"Others"
levels(movies$main_director_name)[levels(movies$main_director_name) %notin% significant_directors ] <-"Others"


levels(movies$main_actor1_name)
levels(movies$main_director_name)

# Re leveling the columns so that others are the base level. We now have all significant actors and directors ready to be used in the model.
movies$main_actor1_name = relevel(movies$main_actor1_name, ref="Others")
movies$main_director_name = relevel(movies$main_director_name, ref="Others")
attach(movies)



#"""Code for final model:"""

mreg <- lm(imdb_score~
             poly(duration_in_hours,4)+
             poly(year_of_release,3)+
             poly(total_number_of_actors,2)+
             month_of_release+
             total_number_languages+
             poly(budget_in_millions,3)+
             poly(total_number_of_producers,4)+
             main_production_country_US+
             genre_action*genre_comedy+
             genre_drama*genre_horror+
             main_actor1_is_female*main_production_country_US+
             budget_in_millions*year_of_release+
             duration_in_hours*genre_comedy+
             main_actor1_name+
             main_director_name)

#summary(mreg)

#stargazer regression table
library(stargazer)
stargazer(mreg, type='html')

#"""The code below shows how we tested the out-of-sample performance of each model we created

#"""

#Calculating MSE using LOOCV
library(boot)

fit=glm(imdb_score~
          poly(duration_in_hours,4)+
          poly(year_of_release,3)+
          poly(total_number_of_actors,2)+
          month_of_release+
          total_number_languages+
          poly(budget_in_millions,3)+
          poly(total_number_of_producers,4)+
          main_production_country_US+
          genre_action*genre_comedy+
          genre_drama*genre_horror+
          main_actor1_is_female*main_production_country_US+
          budget_in_millions*year_of_release+
          duration_in_hours*genre_comedy+
          main_actor1_name+
          main_director_name,   
        data=movies)

mse=cv.glm(movies, fit)$delta[1]
mse

#Calculating MSE using validation-set test,

library(caTools)

MSE = c()

for (i in 1:100) {
  sample=sample.split(movies$imdb_score, SplitRatio=0.80)
  train=subset(movies, sample==TRUE)
  test=subset(movies, sample==FALSE)

  fit = lm(imdb_score~
             poly(duration_in_hours,4)+
            poly(year_of_release,3)+
            poly(total_number_of_actors,2)+
            month_of_release+
            total_number_languages+
            poly(budget_in_millions,3)+
            poly(total_number_of_producers,4)+
            main_production_country_US+
            genre_action*genre_comedy+
            genre_drama*genre_horror+
            main_actor1_is_female*main_production_country_US+
            budget_in_millions*year_of_release+
            duration_in_hours*genre_comedy+
            main_actor1_name+
            main_director_name,
          data = train)

  test$pred=predict(fit,test)
  test$res=(test$imdb_score-test$pred)
  test$res_sq=(test$res)^2
  MSE[i] = mean(test$res_sq)
}
AVG_MSE = mean(MSE)
AVG_MSE


linear_results = test['URR']
linear_results$linear_pred = predict(linear, test, type = 'response')
linear_results$linear_pred_class = ifelse(linear_results$linear_pred>0.5, 1, 0)
linear_results$missclassified = ifelse(linear_results$linear_pred_class == linear_results$URR, 0, 1)
lin_error_rate = mean(linear_results$missclassified)
lin_error_rate

quadratic_results = test['URR']
quadratic_results$quadratic_pred = predict(quadratic, test, type = 'response')
quadratic_results$quadratic_pred_class = ifelse(quadratic_results$quadratic_pred>0.5, 1, 0)
quadratic_results$missclassified = ifelse(quadratic_results$quadratic_pred_class == quadratic_results$URR, 0, 1)
quad_error_rate = mean(quadratic_results$missclassified)
quad_error_rate

segmented_results = test['URR']
segmented_results$segmented_pred = predict(segmented_model, test, type = 'response')
segmented_results$segmented_pred_class = ifelse(segmented_results$segmented_pred>0.5, 1, 0)
segmented_results$missclassified = ifelse(segmented_results$segmented_pred_class == segmented_results$URR, 0, 1)
seg_error_rate = mean(segmented_results$missclassified)
seg_error_rate
