# print aggregate memory usage statistics
print(paste('R is using', memory.size(), 'MB out of limit', memory.limit(), 'MB'))

# create function to return matrix of memory consumption
object.sizes <- function()
{
    return(rev(sort(sapply(ls(envir=.GlobalEnv), function (object.name) 
		object.size(get(object.name))))))
}

# print to console in table format
object.sizes()

# draw bar plot
barplot(object.sizes(), 
	main="Memory usage by object", ylab="Bytes", xlab="Variable name", 
	col=heat.colors(length(object.sizes())))

# draw dot chart
dotchart(object.sizes(), main="Memory usage by object", xlab="Bytes")

# draw pie chart
pie(object.sizes(), main="Memory usage by object")