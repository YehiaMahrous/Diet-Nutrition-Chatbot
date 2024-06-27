## Diet and Nutrition Chatbot
## Overview
This project is a Scala-based diet and nutrition chatbot designed to help users create and manage personalized diet plans. The chatbot analyzes food data, calculates Basal Metabolic Rate (BMR), and generates diet plans based on user profiles.

## Features
Food Data Analysis: Reads and parses food data from a file.
User Profile Management: Supports user profiles including height, weight, activity level, gender, and age.
BMR Calculation: Calculates the Basal Metabolic Rate (BMR) based on user profile details.
Diet Plan Generation: Generates and prints personalized diet plans.
## Installation
To run this project, ensure you have Scala installed on your system. If not, you can download it from Scala's official website.

## Usage
Clone the Repository:

'git clone https://github.com/yehiamahrous/diet-nutrition-chatbot.git'

'cd diet-nutrition-chatbot'

Compile and Run:

'scalac Main.scala'

'scala NutritionChatbot'

## File Structure
Main.scala: The main Scala file containing the chatbot implementation.

## Code Explanation
Data Structures
Food: A case class representing a food item with its nutritional values.
UserProfile: A case class representing the user's profile.

## Functions
AnalyzeFoodData(filePath: String): Reads and parses the food data from the specified file.
parseFoodLine(line: String): Parses a line of food data.
calculateBMR(user: UserProfile): Calculates the Basal Metabolic Rate (BMR) for a user based on their profile.
printPlan(plan: List[Food], mealType: String): Prints the diet plan for a specific meal type.

## Limitations
-No Support for Special Diets: The current implementation does not support creating diet plans for vegans, vegetarians, or individuals with specific dietary restrictions or allergies.
-Limited User Profile Data: The user profile currently only includes height, weight, activity level, gender, and age. Other factors like medical conditions or specific fitness goals are not considered.
-Static Food Data: The chatbot uses a static food data file and does not support dynamic updates or integration with external food databases.

## Future Enhancements
-Add support for creating diet plans for different types of eaters, including vegans and vegetarians.
-Incorporate dietary restrictions and allergy information into the user profile and diet plan generation.
-Integrate with external food databases for dynamic food data updates.
-Include additional user profile attributes such as medical conditions and specific fitness goals.

## Example
To use the chatbot, run the scala 3 code and create a user profile and ask the chatbot for a diet plan, then the bot will ask you for some information to calculate your BMR and create a specified diet plan based on your needs. You can ask the Chatbot to change a specific food from your diet plan if you do not like it.

## Contributing
Contributions are welcome! Please open an issue or submit a pull request for any improvements or bug fixes.
