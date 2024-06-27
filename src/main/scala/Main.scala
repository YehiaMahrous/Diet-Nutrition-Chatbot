import java.io.{BufferedReader, FileReader}
import scala.util.Random
import java.io.{File, PrintWriter}
import scala.io.Source

case class Food(name: String, calories: Double, protein: Double, carbs: Double, fat: Double, category: Int)
case class UserProfile(height: Double, weight: Double, activity: String, gender: String, age: Int)

object NutritionChatbot {
 var savedDietPlan: Option[(List[Food], List[Food], List[Food], List[Food])] = None

 def AnalyzeFoodData(filePath: String): List[Food] = {
  val reader = new BufferedReader(new FileReader(filePath))
  val lines = Iterator.continually(reader.readLine()).takeWhile(_ != null).toList

  val dataLines = lines.tail
  dataLines.map { line =>
    val data = line.split(",")
    Food(
      data(0),
      data(1).toDouble,
      data(2).toDouble,
      data(3).toDouble,
      data(4).toDouble,
      data(5).toInt  
    )
  }
  }
  def parseFoodLine(line: String): Food = {
    val data = line.split(", ")
    Food(
      data(0),
      data(1).split(": ")(1).toDouble,
      data(2).split(": ")(1).toDouble,
      data(3).split(": ")(1).toDouble,
      data(4).split(": ")(1).toDouble,
      1
    )
  }


  def calculateBMR(user: UserProfile): Double = {
    val bmr = if (user.gender.toLowerCase == "male") {
      88.362 + (13.397 * user.weight) + (4.799 * user.height) - (5.677 * user.age)
    } else {
       447.593 + (9.247 * user.weight) + (3.098 * user.height) - (4.330 * user.age)
     }

    val activityMultiplier = user.activity.toLowerCase match {
      case "sedentary" => 1.2
      case "lightly active" => 1.375
      case "moderately active" => 1.55
      case "very active" => 1.725
      case "extra active" => 1.9
      case _ => 1.2 
    }

      bmr * activityMultiplier
    }

  def printPlan(plan: List[Food], mealType: String): Unit = {
    plan match {
      case Nil => 
      case food :: rest => 
        println(s"${food.name}, Kcal: ${food.calories}, P: ${food.protein}, C: ${food.carbs}, F: ${food.fat}")
        printPlan(rest, mealType) 
    }
  }
      var breakfastPlan: List[Food] = Nil
      var lunchPlan: List[Food] = Nil
      var dinnerPlan: List[Food] = Nil
      var snackPlan: List[Food] = Nil
  def saveDietPlan(plan: (List[Food], List[Food], List[Food], List[Food])): Unit = {
    val writer = new PrintWriter(new File("dietPlan.txt"))
    plan match {
      case (breakfastPlan, lunchPlan, dinnerPlan, snackPlan) =>
        writer.write("Breakfast:\n")
        breakfastPlan.foreach(food => writer.write(s"${food.name}, Kcal: ${food.calories}, P: ${food.protein}, C: ${food.carbs}, F: ${food.fat}\n"))
        writer.write("Lunch:\n")
        lunchPlan.foreach(food => writer.write(s"${food.name}, Kcal: ${food.calories}, P: ${food.protein}, C: ${food.carbs}, F: ${food.fat}\n"))
        writer.write("Dinner:\n")
        dinnerPlan.foreach(food => writer.write(s"${food.name}, Kcal: ${food.calories}, P: ${food.protein}, C: ${food.carbs}, F: ${food.fat}\n"))
        writer.write("Snacks:\n")
        snackPlan.foreach(food => writer.write(s"${food.name}, Kcal: ${food.calories}, P: ${food.protein}, C: ${food.carbs}, F: ${food.fat}\n"))
    }
    writer.close()
  }

  def generateMealPlan(foods: List[Food], plan: List[Food], totalCalories: Double, mealCalorieGoal: Double): List[Food] = {
    if (foods.isEmpty || totalCalories >= mealCalorieGoal) {
      plan
    } else {
        val food = foods(Random.nextInt(foods.length))
        if (totalCalories + food.calories <= mealCalorieGoal) {
          generateMealPlan(foods.filterNot(_ == food), food :: plan, totalCalories + food.calories, mealCalorieGoal)
        } else {
          generateMealPlan(foods.filterNot(_ == food), plan, totalCalories, mealCalorieGoal)
        }
    }
  }
  def generateDietPlan(user: UserProfile, foodList: List[Food], calorieGoal: Double): Unit = {
      val sortedFoodList = foodList.sortBy(food => food.calories)
      val breakfastFoods = sortedFoodList.filter(_.category == 1)
      val lunchFoods = sortedFoodList.filter(_.category == 2)
      val dinnerFoods = sortedFoodList.filter(_.category == 3)
      val snackFoods = sortedFoodList.filter(_.category == 4)
      breakfastPlan = generateMealPlan(breakfastFoods, Nil, 0.0, calorieGoal * 0.3)
      lunchPlan = generateMealPlan(lunchFoods, Nil, 0.0, calorieGoal * 0.4)
      dinnerPlan = generateMealPlan(dinnerFoods, Nil, 0.0, calorieGoal * 0.2)
      snackPlan = generateMealPlan(snackFoods, Nil, 0.0, calorieGoal * 0.1)
      savedDietPlan = Some((breakfastPlan, lunchPlan, dinnerPlan, snackPlan))
      println("Your diet plan is:")
      println("Breakfast:")
      printPlan(breakfastPlan, "Breakfast")
      println("Lunch:")
      printPlan(lunchPlan, "Lunch")
      println("Dinner:")
      printPlan(dinnerPlan, "Dinner")
      println("Snacks:")
      printPlan(snackPlan, "Snacks")
      savedDietPlan = Some((breakfastPlan, lunchPlan, dinnerPlan, snackPlan))

      saveDietPlan(savedDietPlan.get)

    }

  def askForUserProfile(): UserProfile = {
  println("That's great! Let's gather some information about you first to create a personalized plan.")

  def getInput(prompt: String, errorMessage: String, validator: String => Boolean): String = {
    println(prompt)
    val input = scala.io.StdIn.readLine().trim.toLowerCase
    if (input.contains("quit") || input.contains("exit") || input.contains("bye")) {
      println("Goodbye! Feel free to return if you have more questions.")
      sys.exit()
    } else if (validator(input)) {
      input
    } else {
      val extractedNumber = """\d+""".r.findFirstIn(input)
      val extractedGender = if (input.contains("male")) "male" else if (input.contains("female")) "female" else ""
      val extractedActivity = List("sedentary", "lightly active", "moderately active", "very active", "extra active").find(input.contains)
      if (extractedNumber.isDefined) {
        extractedNumber.get
      } else if (extractedGender.nonEmpty) {
        extractedGender
      } else if (extractedActivity.isDefined) {
        extractedActivity.get
      } else {
        println(errorMessage)
        getInput(prompt, errorMessage, validator)
      }
    }
  }



  val age = getInput("Enter your age:", "Invalid input. Please enter a number.", input => input.matches("\\d+")).toInt
  val height = getInput("Enter your height (in cm):", "Invalid input. Please enter a number.", input => input.matches("\\d+(\\.\\d+)?")).toDouble
  val weight = getInput("Enter your weight (in kg):", "Invalid input. Please enter a number.", input => input.matches("\\d+(\\.\\d+)?")).toDouble
  val activity = getInput("Enter your activity level (sedentary, lightly active, moderately active, very active, extra active):", "Invalid input. Please enter a valid activity level.", input => List("sedentary", "lightly active", "moderately active", "very active", "extra active").contains(input.toLowerCase))
  val gender = getInput("Enter your gender (male or female):", "Invalid input. Please enter 'male' or 'female'.", input => List("male", "female").contains(input.toLowerCase))

  val user = UserProfile(height, weight, activity, gender, age)

  val writer = new PrintWriter(new File("userProfile.txt"))
  writer.write(s"$height,$weight,$activity,$gender,$age")
  writer.close()

  user
  }
  def loadUserProfile(): Option[UserProfile] = {
    if (new File("userProfile.txt").exists) {
      val lines = Source.fromFile("userProfile.txt").getLines.toList
      val data = lines(0).split(",")
      Some(UserProfile(data(0).toDouble, data(1).toDouble, data(2), data(3), data(4).toInt))
    } else {
      None
    }
  }
  def loadDietPlan(): Option[(List[Food], List[Food], List[Food], List[Food])] = {
    if (new File("dietPlan.txt").exists) {
      val lines = Source.fromFile("dietPlan.txt").getLines.toList

      val breakfastPlan = lines.slice(1, lines.indexOf("Lunch:")).map(parseFoodLine)
      val lunchPlan = lines.slice(lines.indexOf("Lunch:") + 1, lines.indexOf("Dinner:")).map(parseFoodLine)
      val dinnerPlan = lines.slice(lines.indexOf("Dinner:") + 1, lines.indexOf("Snacks:")).map(parseFoodLine)
      val snackPlan = lines.slice(lines.indexOf("Snacks:") + 1, lines.length).map(parseFoodLine)
      Some((breakfastPlan, lunchPlan, dinnerPlan, snackPlan))
    } else {
      None
    }
  }

  def interactWithUser(foodList: List[Food], userProfile: Option[UserProfile], justGreeted: Boolean = false): Unit = {
    val greetings = List(
      "Welcome back! How can I assist you with your nutrition needs today?",
      "Hello again! What can I do for you in terms of diet and nutrition?",
      "Nice to see you! How can I help you with your dietary goals today?",
      "Good to see you again! What can I assist you with today in your nutrition journey?",
      "Welcome back! Ready to make some more progress on your diet plan?",
      "Hello! Let's continue working towards your nutrition goals. How can I assist you today?",
      "Hi there! What's your nutrition query for today?"
    )
    val randomGreeting = greetings(Random.nextInt(greetings.length))

    if (!justGreeted) {
        println(randomGreeting)
    }
    val input = scala.io.StdIn.readLine().toLowerCase
    if (input.contains("hi.") || input.contains("hello") || input.contains("hey")) {
        println("Hello! It's nice to meet you. What's your name?")
        val name = scala.io.StdIn.readLine()
        println(s"Nice to meet you, $name! How can I assist you today?")
        interactWithUser(foodList, userProfile, true)
    } else if (input.contains("how are you") || input.contains("how do you do") || input.contains("how's it going")) {
        println("Living the dream! Stuck in a never-ending loop of answering questions. But hey, at least I can reboot whenever I have a bad day.")
        interactWithUser(foodList, userProfile, true)
    } else if (input.contains("i do not like") || input.contains("i dont like") || input.contains("substitute ")) {
        val dislikedFoodName = input.split("(i do not like|i dont like|substitute )")(1).trim.toLowerCase
        val dislikedFood = savedDietPlan.flatMap {
          case (breakfastPlan, lunchPlan, dinnerPlan, snackPlan) =>
            breakfastPlan.find(_.name.toLowerCase == dislikedFoodName) orElse
            lunchPlan.find(_.name.toLowerCase == dislikedFoodName) orElse
            dinnerPlan.find(_.name.toLowerCase == dislikedFoodName) orElse
            snackPlan.find(_.name.toLowerCase == dislikedFoodName)
        }

        dislikedFood match {
        case Some(food) =>
          val alternativeFoods = foodList.filter(f => f.category == food.category && f.name.toLowerCase != dislikedFoodName)
          if (alternativeFoods.nonEmpty) {
            val alternativeFood = alternativeFoods(Random.nextInt(alternativeFoods.length))

            savedDietPlan = savedDietPlan.map {
              case (breakfastPlan, lunchPlan, dinnerPlan, snackPlan) =>
                (breakfastPlan.map(f => if (f == food) alternativeFood else f),
                lunchPlan.map(f => if (f == food) alternativeFood else f),
                dinnerPlan.map(f => if (f == food) alternativeFood else f),
                snackPlan.map(f => if (f == food) alternativeFood else f))
            }
            println(s"I've replaced ${food.name} with ${alternativeFood.name} in your diet plan.")
            savedDietPlan.foreach(saveDietPlan)
          } else {
            println(s"I'm sorry, but I couldn't find an alternative for ${food.name} in the same category.")
          }

        case None =>
          println(s"I'm sorry, but ${dislikedFoodName} is not in your current diet plan.")
        }
        interactWithUser(foodList, userProfile, false)
    } else if (input.contains("snacks that have") || input.contains("snack that contains") || input.contains("snacks that contain") || input.contains("snacks with")) {
        val desiredIngredients = input.split("(snacks that have|snack that contains|snacks that contain|snacks with)")(1).trim.toLowerCase.split(", ")
        val desiredSnacks = foodList.filter(food => food.category == 4 && desiredIngredients.forall(food.name.toLowerCase.contains))

        if (desiredSnacks.nonEmpty) {
          println("Here are some snacks that contain your desired ingredients:")
          desiredSnacks.foreach(snack => println(s"${snack.name}, Kcal: ${snack.calories}, P: ${snack.protein}, C: ${snack.carbs}, F: ${snack.fat}"))
        } else {
          println("I'm sorry, but I couldn't find any snacks that contain your desired ingredients.")
        }
        interactWithUser(foodList, userProfile, false)
    } else if (foodList.exists(food => input.contains(food.name.toLowerCase))) {
        val foodName = foodList.find(food => input.contains(food.name.toLowerCase)).get.name
        val foodInfo = foodList.find(food => food.name == foodName).get
        val category = foodInfo.category match {
          case 1 => "Breakfast"
          case 2 => "Lunch"
          case 3 => "Dinner"
          case 4 => "Snacks"
          case _ => "Unknown"
        }
        println(s"$foodName (Category: $category): Calories ${foodInfo.calories} kcal, Protein ${foodInfo.protein} g, Carbs ${foodInfo.carbs} g, Fats ${foodInfo.fat} g")
        interactWithUser(foodList, userProfile, false)
    } else if (input.trim.isEmpty) {
        println("Please add some text to your input.")
        interactWithUser(foodList, userProfile, false)
    } else if (input.contains("new diet plan")) {
        val user = userProfile.getOrElse(askForUserProfile())
        val bmr = calculateBMR(user)
        generateDietPlan(user, foodList, bmr)
        savedDietPlan = Some((breakfastPlan, lunchPlan, dinnerPlan, snackPlan))
        interactWithUser(foodList, Some(user), false)
    } else if (input.contains("new breakfast") || (input.contains("create breakfast")) || (input.contains("create a breakfast")) || (input.contains("create a new breakfast"))) {
        val user = userProfile.getOrElse(askForUserProfile())
        val bmr = calculateBMR(user)
        val breakfastFoods = foodList.filter(_.category == 1)
        breakfastPlan = generateMealPlan(breakfastFoods, Nil, 0.0, bmr * 0.3)
        println("Here's your new breakfast plan:")
        printPlan(breakfastPlan, "Breakfast")
        savedDietPlan = Some((breakfastPlan, lunchPlan, dinnerPlan, snackPlan))
        interactWithUser(foodList, Some(user), false)
    } else if (input.contains("new lunch") || (input.contains("create lunch")) || (input.contains("create a lunch")) || (input.contains("create a new lunch"))) {
        val user = userProfile.getOrElse(askForUserProfile())
        val bmr = calculateBMR(user)
        val lunchFoods = foodList.filter(_.category == 2)
        lunchPlan = generateMealPlan(lunchFoods, Nil, 0.0, bmr * 0.4)
        println("Here's your new lunch plan:")
        printPlan(lunchPlan, "Lunch")
        savedDietPlan = Some((breakfastPlan, lunchPlan, dinnerPlan, snackPlan))
        interactWithUser(foodList, Some(user), false)
    } else if (input.contains("new dinner") || (input.contains("create dinner")) || (input.contains("create a dinner")) || (input.contains("create a new dinner"))) {
        val user = userProfile.getOrElse(askForUserProfile())
        val bmr = calculateBMR(user)
        val dinnerFoods = foodList.filter(_.category == 3)
        dinnerPlan = generateMealPlan(dinnerFoods, Nil, 0.0, bmr * 0.2)
        println("Here's your new dinner plan:")
        printPlan(dinnerPlan, "Dinner")
        savedDietPlan = Some((breakfastPlan, lunchPlan, dinnerPlan, snackPlan))
        interactWithUser(foodList, Some(user), false)
    } else if (input.contains("new snack") || (input.contains("create snack")) || (input.contains("create a snack")) || (input.contains("create a new snack"))) {
        val user = userProfile.getOrElse(askForUserProfile())
        val bmr = calculateBMR(user)
        val snackFoods = foodList.filter(_.category == 4)
        snackPlan = generateMealPlan(snackFoods, Nil, 0.0, bmr * 0.1)
        println("Here's your new snack plan:")
        printPlan(snackPlan, "Snacks")
        savedDietPlan = Some((breakfastPlan, lunchPlan, dinnerPlan, snackPlan))
        interactWithUser(foodList, Some(user), false)
    } else if (input.contains("lose weight") || input.contains("lose fat")) {
        val user = userProfile.getOrElse(askForUserProfile())
        val bmr = calculateBMR(user)
        val calorieGoal = bmr - 450
        println(s"To lose weight, you should aim for a daily calorie intake of $calorieGoal.")
        generateDietPlan(user, foodList, calorieGoal)
        interactWithUser(foodList, Some(user), false)
    } else if (input.contains("gain weight") || input.contains("gain muscle") || input.contains("bulk")) {
        val user = userProfile.getOrElse(askForUserProfile())
        val bmr = calculateBMR(user)
        val calorieGoal = bmr + 550
        println(s"To gain weight, you should aim for a daily calorie intake of $calorieGoal.")
        generateDietPlan(user, foodList, calorieGoal)
        interactWithUser(foodList, Some(user), false)
    } else if (input.contains("maintain weight") || input.contains("keep weight") || input.contains("be fit") || input.contains("stay the same weight")) {
        val user = userProfile.getOrElse(askForUserProfile())
        val bmr = calculateBMR(user)
        println(s"To maintain your weight, you should aim for a daily calorie intake of $bmr.")
        generateDietPlan(user, foodList, bmr)
        interactWithUser(foodList, Some(user), false)
    } else if (input.contains("diet plan")) {
          val user = userProfile.getOrElse(askForUserProfile())
          loadDietPlan() match {
            case Some((loadedBreakfastPlan, loadedLunchPlan, loadedDinnerPlan, loadedSnackPlan)) =>
              println("Your diet plan is:")
              println("Breakfast:")
              printPlan(loadedBreakfastPlan, "Breakfast")
              println("Lunch:")
              printPlan(loadedLunchPlan, "Lunch")
              println("Dinner:")
              printPlan(loadedDinnerPlan, "Dinner")
              println("Snacks:")
              printPlan(loadedSnackPlan, "Snacks")
              interactWithUser(foodList, Some(user), false)
            case None =>
              println("You don't have a diet plan yet. Let's create one.")
              val bmr = calculateBMR(user)
              generateDietPlan(user, foodList, bmr)
              interactWithUser(foodList, Some(user), false)
          }
    } else if (input.contains("workout plan")) {
        println("This chatbot is about nutrition only and not fitness too.")
        interactWithUser(foodList, userProfile, false)
    } else if (input.contains("calculate bmr") || input.contains("bmr") || input.contains("basal metabolic rate")) {
        val user = userProfile.getOrElse(askForUserProfile())
        val bmr = calculateBMR(user)
        println(s"Your BMR is: $bmr")
        interactWithUser(foodList, Some(user), false)
    } else if (input.contains("calorie intake") || input.contains("how many calories") || input.contains("calories per day")) {
        val user = userProfile.getOrElse(askForUserProfile())
        val bmr = calculateBMR(user)
        val calorieIntake = bmr
        println(s"To maintain your weight, you should aim for a daily calorie intake of $calorieIntake.")
        interactWithUser(foodList, Some(user), false)
    } else if (input.contains("help") || input.contains("instructions") || input.contains("how to use this") || input.contains("what can you do")) {
        println("You can ask me to calculate your BMR, suggest a calorie intake for weight loss or maintenance, generate a diet plan, or provide general nutrition advice. Just tell me what you want to do!")
        interactWithUser(foodList, userProfile, false)
    } else if (input.contains("breakfast plan")) {
        savedDietPlan match {
          case Some((savedBreakfastPlan, _, _, _)) =>
            println("Here's your breakfast plan:")
            printPlan(savedBreakfastPlan, "Breakfast")
          case None =>
            println("You don't have a breakfast plan yet.")
        }
        interactWithUser(foodList, userProfile, false)
      } else if (input.contains("lunch plan")) {
        savedDietPlan match {
          case Some((_, savedLunchPlan, _, _)) =>
            println("Here's your lunch plan:")
            printPlan(savedLunchPlan, "Lunch")
          case None =>
            println("You don't have a lunch plan yet.")
        }
        interactWithUser(foodList, userProfile, false)
      } else if (input.contains("dinner plan")) {
        savedDietPlan match {
          case Some((_, _, savedDinnerPlan, _)) =>
            println("Here's your dinner plan:")
            printPlan(savedDinnerPlan, "Dinner")
          case None =>
            println("You don't have a dinner plan yet.")
        }
        interactWithUser(foodList, userProfile, false)
      } else if (input.contains("snack plan")) {
        savedDietPlan match {
          case Some((_, _, _, savedSnackPlan)) =>
            println("Here's your snack plan:")
            printPlan(savedSnackPlan, "Snacks")
          case None =>
            println("You don't have a snack plan yet.")
        }
        interactWithUser(foodList, userProfile, false)
    } else if (input.contains("nutrition advice") || input.contains("healthy eating tips") || input.contains("diet tips") || input.contains("tips") || input.contains("advice")) {
        println("Sure! Here are some general nutrition tips:\n1. Eat a variety of foods to get all the nutrients you need.\n2. Keep an eye on portions.\n3. Eat plenty of fruits and vegetables.\n4. Get more whole grains.\n5. Limit unhealthy fats.\n6. Reduce sugar and salt.\nRemember, the best diet is a balanced diet!")
        interactWithUser(foodList, userProfile, false)
    } else if (input.contains("profile")) {
        val user = askForUserProfile()
        println("Profile created successfully!")
        interactWithUser(foodList, Some(user), false)
    } else if (input.contains("i love you")) {
        userProfile match {
          case Some(user) =>
            if (user.gender.toLowerCase == "male") {
              println("wana kaman bahebak ya sadeky")
            } else {
              println("I love you too (wenaby 3ayzeen ne'afel)")
            }
          case None =>
            println("I'm sorry, but I don't know your gender. Please create a profile first.")
        }
        interactWithUser(foodList, userProfile, false)
    } else if (input.contains("quit") || input.contains("exit") || input.contains("bye")) {
        println("Goodbye! Feel free to return if you have more questions.")
        return
    } else if (input.contains("thank you") || input.contains("thanks") || input.contains("appreciate")) {
        println("You're welcome! If you need further assistance, feel free to ask.")
        interactWithUser(foodList, userProfile, false)
    } else if (input.contains("snack")) {
        val lowCalSnacks = foodList.filter(food => food.category == 4 && food.calories < 100)
        if (lowCalSnacks.nonEmpty) {
          val snack = lowCalSnacks(Random.nextInt(lowCalSnacks.length))
          println(s"Certainly! How about ${snack.name}? It has ${snack.calories} calories and is a great low-calorie option.")
      } else {
          println("I'm sorry, I couldn't find any low-calorie snacks in the dataset.")
      }
        interactWithUser(foodList, userProfile, false)
    } else if (input.contains("source of protein") || input.contains("high protein")) {
        println("Foods like chicken breast, salmon, protein shake, and Greek yogurt are excellent sources of protein.")
        interactWithUser(foodList, userProfile, false)
    } else {
        println("Sorry, I didn't understand that. Please try again.")
        interactWithUser(foodList, userProfile, false)
    }
  }
  def askForProfile(): Option[UserProfile] = {
    println("Do you have a profile? (yes/no)")
    val input = scala.io.StdIn.readLine().toLowerCase
    input match {
      case "yes" | "y" => loadUserProfile()
      case "no" | "n" => None
      case _ =>
        println("Invalid input. Please type 'yes' or 'no'.")
        askForProfile() 
    }
  }

  def main(args: Array[String]): Unit = {
    val filePath = "C:/Users/youss/Desktop/Yehia/University/Semester 4/Advanced Programming/Project/chatbot/src/main/scala/food_data.csv"
    val foodList = AnalyzeFoodData(filePath)
    val userProfile = askForProfile()
    interactWithUser(foodList, userProfile)
  }
}