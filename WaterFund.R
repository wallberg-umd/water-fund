library(RSQLite)
library(lubridate)
library(plyr)
library(ggplot2)

source('multiplot.R')

# Connect to exported GnuCash SQLite database and read in tables
conn <- dbConnect(SQLite(), dbname = 'export.gnucash')
dbListTables(conn)

accounts     = dbReadTable(conn, 'accounts')
transactions = dbReadTable(conn, 'transactions')
splits       = dbReadTable(conn, 'splits')

dbDisconnect(conn)

# Read in bottle data
bottles = read.csv(file='bottles.csv')

# Massage data
accounts.contributions = accounts[accounts$name == 'Contributions',]

accounts.contributors = accounts[accounts$parent_guid == accounts.contributions$guid,]
accounts.contributors = accounts.contributors[complete.cases(accounts.contributors$name),]

accounts.water = accounts[accounts$name == 'Water',]

accounts.water.fund = accounts[accounts$name == 'Water Fund',]

splits$value = splits$value_num / splits$value_denom

transactions$post_date = as.POSIXct(strptime(substr(transactions$post_date,1,8), "%Y%m%d"))
transactions$date_month = floor_date(transactions$post_date, "month")

splits.water = splits[splits$account_guid == accounts.water$guid,]

water = data.frame(
  date_month = transactions[transactions$guid %in% splits.water$tx_guid,]$date_month,
  value = splits.water$value
  )

water.monthly = data.frame(
  date_month = unique(water$date_month),
  cost = sapply(split(water, water$date_month), function(x) {sum(x$value)})
)

splits.contributors = splits[splits$account_guid %in% accounts.contributors$guid,]

contributors = data.frame(
  date_month = transactions[transactions$guid %in% splits.contributors$tx_guid,]$date_month,
  contributor = splits.contributors$account_guid,
  value = splits.contributors$value
)

contributors.monthly = data.frame(
  date_month = unique(contributors$date_month),
  contributors = sapply(split(contributors, contributors$date_month), function(x) {length(unique((x$contributor)))}),
  contribution = sapply(split(contributors, contributors$date_month), function(x) {-1 * sum(x$value)})
)

bottles$date = as.POSIXct(bottles$date)
bottles$date_month = floor_date(bottles$date, "month")

bottles.monthly = data.frame(
  date_month = unique(bottles$date_month),
  bottles = sapply(split(bottles, bottles$date_month), function(x) {sum(x$bottles)})
)

monthly = merge(water.monthly, bottles.monthly, all.x=TRUE)
monthly = merge(monthly, contributors.monthly, all.x=TRUE)

# Generate plots
p1 = ggplot(monthly, aes(date_month,cost)) +
  geom_line() + 
  geom_smooth(method='loess') + 
  labs(x='Month', y='Cost ($)') +
  ggtitle('Cost (Bottles + Dispenser)')

p2 = ggplot(monthly, aes(date_month,bottles)) +
  geom_line() + 
  geom_smooth(method='loess') + 
  labs(x='Month', y='Bottles (#)') +
  ggtitle('Number of Bottles Delivered')

p3 = ggplot(monthly, aes(date_month, cost / bottles)) +
  geom_line() + 
  geom_smooth(method='loess') + 
  labs(x='Month', y='Cost ($/Bottle)') +
  ggtitle('Cost (Bottles + Dispenser) per Bottle')

p4 = ggplot(monthly, aes(date_month, contributors)) +
  geom_line() + 
  geom_smooth(method='loess') + 
  labs(x='Month', y='Contributors (#)') +
  ggtitle('Number of Contributors')

p5 = ggplot(monthly, aes(date_month, contribution / contributors)) +
  geom_line() + 
  geom_smooth(method='loess') + 
  labs(x='Month', y='Contribution ($/Contributor)') +
  ggtitle('Average Contribution per Contributor')

multiplot(p1, p2, p3, p4, p5, col=1)

