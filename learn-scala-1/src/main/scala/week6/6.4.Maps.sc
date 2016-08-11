val romanNumerals = Map("I" -> 1, "V" -> 5, "X" -> 10)
val capitalOfCountry = Map("US" -> "Washington", "Switzerland" -> "Bern")

capitalOfCountry("US")

//capitalOfCountry("Andorea")

capitalOfCountry get "Andorea"

capitalOfCountry get "US"

def showCapital(country: String) = capitalOfCountry.get(country) match {
  case Some(capital) => capital
  case None => "missing data"
}

showCapital("US")
showCapital("Andorra")

val fruit = List("apple", "pear", "orange", "pineapple")
fruit sortWith(_.length < _.length)
fruit.sorted

fruit groupBy( _.head)

//x^3 - 2x + 5
Map(0 -> 5, 1 -> -2, 3 -> 1)