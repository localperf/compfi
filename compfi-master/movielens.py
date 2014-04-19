

#---movielens


os.chdir(directory)

unames = ["user_id", "gender", "age", "occupation", "zip"]
users     = pd.read_table("users.dat", sep = "::", header = None, 
    names = unames)
    
    
rnames = ["user_id", "movie_id", "rating", "timestamp"]
ratings     = pd.read_table("ratings.dat", sep = "::", header=None,
    names = rnames)
    
    
mnames = ["movie_id", "title", "genres"]
movies = pd.read_table("movies.dat", sep = "::", header = None,
    names = mnames)


print "\n", users     [:5]
print "\n", ratings   [:5]
print "\n", movies    [:5]

print "\n\tread ok"

###----------------------

data = pd.merge(pd.merge(ratings, users), movies)

print "\n", data[:5]

mean_ratings = data.pivot_table('rating', rows='title', cols = 'gender', 
    aggfunc = 'mean')
    
print "\n", mean_ratings[:5]

ratings_by_title = data.groupby('title').size()
print "\n", ratings_by_title[:10]

active_titles = ratings_by_title.index[ratings_by_title > 250]
print "\n", active_titles[:10]

mean_ratings = mean_ratings.ix[active_titles]
print "\n", mean_ratings[:10]

top_female_ratings = mean_ratings.sort_index(by = "F", ascending = False)
print "\n", top_female_ratings[:10] 

mean_ratings['diff'] = mean_ratings["M"] - mean_ratings["F"]
sorted_by_diff = mean_ratings.sort_index(by = "diff")
print "\n", sorted_by_diff[:15]









