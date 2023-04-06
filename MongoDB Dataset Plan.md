# MongoDB Dataset Plan 

**sample\_analytics**

**accounts** 

\_id : ObjectId which can be used to identify this entry in the accounts dataset

account\_id : AccountID which can be used to link this account to the customers and transactions dataset

limit : integer which may be the daily withdrawal limit on the account (unsure)

products : array, which may contain any of the following strings: “Derivatives”, “InvestmentStock”, “Commodity”, “Brokerage”, “CurrencyService”, “InvestmentFund”

**customers** 

\_id : ObjectId which can be used to identify this entry in the accounts dataset

username : username String

name: full name String

address : address String

birthdate : birthdate Date

email : email String

active : account status Boolean 

accounts : Array of Integers containing account\_id(s) belonging to this account

tier\_and\_details : Object which contains an \_id for each account that the user has

`	`\_id : Object containing the account’s tier and details

`		`tier : tier String

`		`id: ID String  

`		`active : account status Boolean 

benefits : User account benefits Array

`	`0, 1, 2 … : User benefits string (e.g. “sports tickets”)




**transactions** 

\_id : ObjectId which can be used to identify this entry in the accounts dataset

account\_id : AccountID which can be used to link this account to the customers and transactions dataset

transaction\_count : transaction count Integer

bucket\_start\_date: Possibly start of recorded transactions Date (unsure)

bucket\_end\_date : Possibly end of recorded transactions Date (unsure)

transactions : An Array of Objects, where each object is a transaction

`	`date: transaction Date

`	`amount : amount of units in transaction Integer

`	`transaction\_code : transaction code (“buy”, “sell”) String

`	`symbol : the company’s symbol on the stock market (e.g. Apple = AAPL) String

`	`price : price of one unit String

`	`total : total transaction amount String






