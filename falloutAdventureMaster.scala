import io.StdIn._
import scala.util.Random

/*
Authors: Omar Rosario, Matt Gumprecht, John Roslin

Description: This is a text-based adventure based off the video game "Fallout 4."
This program uses if-statements, while-statments, do/while-statements,
matching, Arrays, utlizing recursion and higher-order methods. Moreover,
the program features a working menu with additional functions, and a turn-based
battle sequence. This program is very text heavy and delay commands were added to
make the program feel like a game and to ensure the user reads every line
comfortably.

To remove delay commands, press "CTRL + F," type "Thread" in the first box
and then type "//Thread" in the second and click "Replace All."

The game starts at LINE 459 or CTRL+F "//START."

Thank you
*/

//A collection of responses if the user enters an incorrect answer
def wrongAnswer():Unit = {
  val response = Array(
    s"\n$protagName: Uhh... What the hell did I just say?\n",
    s"\n$protagName: It isn't a hard question...)\n",
    s"\n$protagName: Uhhh... What?\n",
    s"\n$protagName: Was that even an answer?\n",
    s"\n$protagName: It's a not a hard question...\n",
    s"\n$protagName: Uhh... My brain...\n",
    s"\n$protagName: I should pay attention to what I say...\n",
    s"\n$protagName: I don't think that was the correct response...\n",
    s"\n$protagName: I have to get it right the first time...\n",
    s"\n$protagName: I must be tired or something...\n"
  )

  println(response(new Random().nextInt(response.length - 1)))
}

//Provides bonus damage based off equipped weapon. Adds to randomized damage value
def weaponDam():Int = {
  handEq match {
    case "10mm pistol"  => (math.random * (10-8) + 8).toInt
    case "combat rifle" => (math.random * (25-18) + 18).toInt
    case "shotgun"      => (math.random * (30-20) + 20).toInt
    case _              => 0
  }
}

//Provides bonus defense based off equipped armor. Adds to randomized defense value
def protec():Int = {
  armor match {
    case "vault suit"  => 5
    case "vault armor" => 15
    case "metal armor" => 20
    case _ => 0
  }
}

/*
A function that operates as a round-based battle using a while-loop. Enemy stats
are determined by a match statement, providing health and attack damage. Each round
is displayed so the user can determine his/her next choice. The player has the choice
to either attack, defend, heal, or flee. If the user's health reaches 0 or below
the program ends. If the enemy's health reaches 0 or below the
battle sequence will end and the program will continue where it left off.
*/
def battle():String = {
  var npcAttack = 0
  var npcHealth = 0

  localNpc match {
    case "radroach"  =>  npcHealth = 20
    case "bandit"    =>  npcHealth = 40
    case "mirelurk"  =>  npcHealth = 40
    case "ghoul"     =>  npcHealth = 35
    case "yao guai"  =>  npcHealth = 50
    case "deathclaw" =>  npcHealth = 100
  }

  Thread.sleep(2000)
  println(Console.CYAN + s"\nA $localNpc approaches." + Console.WHITE + "\n")
  Thread.sleep(2000)

  do {
    var healLeft = ("")
    if (inventory.count(_ == "stimpack") > 1) {
      healLeft = "(You have: " + Console.CYAN + inventory.count(_ == "stimpack") + Console.WHITE + " stimpacks left)"
    } else if (inventory.count(_ == "stimpack") == 1) {
      healLeft = "(You have: " + Console.CYAN + inventory.count(_ == "stimpack") + Console.WHITE + " stimpack left)"
    } else if (inventory.count(_ == "stimpack") == 0) {
      healLeft = "(" + Console.CYAN + "You have no stimpacks left" + Console.WHITE + ")"
    }

    println(s"Your health: " + Console.GREEN + s"$health" + Console.WHITE + "/" + Console.GREEN + s"$maxHealth" + Console.WHITE)
    Thread.sleep(700)
    println(s"$localNpc health: " + Console.GREEN + s"$npcHealth" + Console.WHITE + "\n")
    Thread.sleep(700)
    println("What will you do?")
    println(s"1. attack (" + Console.CYAN + handEq + Console.WHITE + ")")
    println("2. block (" + Console.CYAN + armor + Console.WHITE + ")")
    println(s"3. heal $healLeft")
    println("4. flee\n")
    var playerTurn = readInt

    localNpc match {
      case "radroach"  =>  npcAttack = (math.random * (10-5) + 5).toInt
      case "bandit"    =>  npcAttack = (math.random * (25-5) + 5).toInt
      case "mirelurk"  =>  npcAttack = (math.random * (18-5) + 5).toInt
      case "ghoul"     =>  npcAttack = (math.random * (15-5) + 5).toInt
      case "yao guai"  =>  npcAttack = (math.random * (18-5) + 5).toInt
      case "deathclaw" =>  npcAttack = (math.random * (30-5) + 5).toInt
    }

    var playerDamage = (math.random * (attack - 1) + 1).toInt + weaponDam()
    var defenseAdjust = (math.random * (defense - 5) + 5).toInt + protec()

    if (playerTurn == 1) {
      npcHealth -= playerDamage

      if (npcHealth <= 0) {
        println(s"\nYou hit the $localNpc for " + Console.RED + s"$playerDamage damage" + Console.WHITE)
        Thread.sleep(700)
      } else {
        armor match {
          case "vault suit"  => npcAttack -= 2
          case "vault armor" => npcAttack -= 5
          case "metal armor" => npcAttack -= 10
          case _ => npcAttack -= 0
        }

        health -= npcAttack
        println(s"\nYou hit the $localNpc for " + Console.RED + s"$playerDamage damage" + Console.WHITE)
        Thread.sleep(700)
        println(s"The $localNpc inflicts: " + Console.RED + s"$npcAttack damage" + Console.WHITE + "\n")
        Thread.sleep(700)
      }

    } else if (playerTurn == 2) {
      var npcDamageOnBlock = npcAttack - (defense * (defenseAdjust * 0.01)).toInt
      var damageBlock = 0

        if (npcDamageOnBlock > 0) {
          damageBlock = (defense * (defenseAdjust * 0.01)).toInt
        } else if (npcDamageOnBlock <= 0) {
          damageBlock = npcAttack
          npcDamageOnBlock = 0
        }

      println(s"\nThe $localNpc attacks you for: " + Console.RED + s"$npcAttack damage" + Console.WHITE)
      Thread.sleep(700)
      println(s"You blocked: " + Console.YELLOW + s"$damageBlock damage " + Console.WHITE + s"from the $localNpc")
      Thread.sleep(700)
      println("You took: " + Console.RED +  s"$npcDamageOnBlock damage" + Console.WHITE + "\n")
      Thread.sleep(700)

      health -= npcDamageOnBlock.toInt

    } else if (playerTurn == 3) {
      println(heal())

      armor match {
        case "vault suit"  => npcAttack -= 2
        case "vault armor" => npcAttack -= 5
        case "metal armor" => npcAttack -= 10
        case _ => npcAttack -= 0
      }

      health -= npcAttack
      println(s"\nThe $localNpc attacks you for: " + Console.RED + s"$npcAttack damage" + Console.WHITE + "\n")
      Thread.sleep(700)
    } else if (playerTurn == 4) {
      println("\nYou're a coward!\n")
      Thread.sleep(700)
      health -= npcAttack
      println(s"\nThe $localNpc attacks you for: " + Console.RED + s"$npcAttack damage" + Console.WHITE + "\n")
      Thread.sleep(700)
    } else if (playerTurn < 1 || playerTurn > 4) {
      health -= npcAttack
      println(s"$protagName: Oh no!")
      Thread.sleep(700)
      println(s"\nThe $localNpc attacks you for: " + Console.RED + s"$npcAttack damage" + Console.WHITE + "\n")
      Thread.sleep(700)
    }
  } while (health > 0 && npcHealth > 0)

  if (health <= 0) {
    println("You're dead.")
    Thread.sleep(2000)
    println("Don't do that.\n")
    Thread.sleep(2000)
    println(Console.MAGENTA + "GAME OVER" + Console.WHITE)
    Thread.sleep(3000)
    System.exit(1)
  } else if (npcHealth <= 0) {
    println(Console.GREEN + s"\nYou have killed the $localNpc" + Console.WHITE)
    Thread.sleep(5000)
  }
""
}

//Calculates player health
def hitPoints(endurance:Int, luck:Int):Int = {
  var health = endurance * 10

  if (luck > 5) {
    health += 5
  } else if (luck < 5) {
    health -= 5
  }

  health
}

//Calculates player damage
def damage(strength:Int, endurance:Int, intelligence:Int, luck:Int):Int = {
  var damage = (strength * endurance) + intelligence

  if (luck > 5) {
    damage += 5
  } else if (luck < 5) {
    damage -= 5
  }

  damage
}

//Calculates player defense
def block(endurance:Int, agility:Int, perception:Int, luck:Int):Int = {
  var block = (endurance * agility) + perception

  if (luck > 5) {
    block += 5
  } else if (luck < 5) {
    block -= 5
  }

  block
}

//A cosmetic function. It states the amount of points added to overall player damage
def weaponBonusId(handEq:String) = {
  handEq match {
    case "10mm pistol"  => "8-10"
    case "combat rifle" => "18-25"
    case "shotgun"      => "20-30"
    case _              => "0"
  }
}

//A cosmetic function. It states the amount of points added to overall player defense
def armorBonusId(armor:String) = {
  armor match {
    case "vault suit"  => "5"
    case "vault armor" => "15"
    case "metal armor" => "20"
    case _             => "0"
  }
}

//A cosmetic function. It states the amount of damage negated by enemy
def threshold(armor:String):String = {
  armor match {
    case "vault suit"  => "2"
    case "vault armor" => "5"
    case "metal armor" => "10"
    case _             => "0"
  }
}

//Create user interface element that lets the player view their stats
def viewSpec():String = {
  println(Console.MAGENTA + "\nPlease press \"enter\" to view S.P.E.C.I.A.L and stats or type \"done\" to exit.\n" + Console.WHITE)
  val view = readLine

  if (view == "done") {
    ""
  } else {
    println(s"> strength: $strength")
    Thread.sleep(100)
    println(s"> peception: $perception")
    Thread.sleep(100)
    println(s"> endurance: $endurance")
    Thread.sleep(100)
    println(s"> charisma: $charisma")
    Thread.sleep(100)
    println(s"> intelligence: $intelligence")
    Thread.sleep(100)
    println(s"> agility: $agility")
    Thread.sleep(100)
    println(s"> luck: $luck\n")
    Thread.sleep(100)
    println(s"> Health: $health/$maxHealth  (Damage Ignored: " + threshold(armor) + ")")
    Thread.sleep(100)
    println(s"> Attack: $attack  (Weapon Bonus: " + weaponBonusId(handEq) + ")")
    Thread.sleep(100)
    println(s"> Defense: $defense (Armor Bonus: " + armorBonusId(armor) + ")\n")
    Thread.sleep(100)
    viewSpec()
  }
}

//Create user interface element that lets the player permanately remove an item
def drop():Unit = {
  var exit = false
  var itemDrop = 0
  var empty = ("")

  do {
    println(Console.MAGENTA + "\nType what you want to drop. Type \"done\" when finished.\n" + Console.WHITE)
    println(s"Equipped: $handEq")
    println(s"Body: $armor")
    print("\n")
    println("Backpack:")
    println(inventory.mkString("\n"))
    print("\n")
    val giveAway = readLine

    if (giveAway == handEq) {
      handEq = "-"
      println(Console.CYAN + "\n" + giveAway.toUpperCase + " REMOVED" + Console.WHITE)
      itemDrop += 1
      Thread.sleep(2000)
    } else if (giveAway == armor) {
      armor = "-"
      println(Console.CYAN + "\n" + giveAway.toUpperCase + " REMOVED" + Console.WHITE)
      itemDrop += 1
      Thread.sleep(2000)
    } else if (inventory.contains(giveAway) == true) {
      inventory((inventory.indexOf(giveAway))) = "-"
      println(Console.CYAN + "\n" + giveAway.toUpperCase + " REMOVED" + Console.WHITE)
      itemDrop += 1
      Thread.sleep(2000)
    } else if (inventory.contains(giveAway) == false && giveAway != "done") {
      println(Console.CYAN + s"\n$protagName: Can't remove something I don't have..." + Console.WHITE)
      Thread.sleep(2000)
    } else if (giveAway == "done") {
      exit = true
    }
  } while (!exit)

  itemDrop match {
    case x if (x > 1)  => println(Console.CYAN + "\nITEMS REMOVED" + Console.WHITE)
    case x if (x == 1) => println(Console.CYAN + "\nITEM REMOVED" + Console.WHITE)
    case x if (x <= 0) => println(Console.CYAN + "\nDROP CANCELED" + Console.WHITE)
  }
}

//Allows player to automatically store items into storage array
def pickUp(inventory:Array[String], localItem:String):Unit = {
  if (inventory.contains("-") == true) {
    inventory(inventory.indexOf("-")) = localItem
    println(Console.CYAN + localItem.toUpperCase + " ADDED" + "\n" +  Console.WHITE)
    Thread.sleep(2000)
  } else if (inventory.contains("-") == false) {
    println(s"\n$protagName:" + Console.CYAN + " Damn. My backpack is full...\n" + Console.WHITE)
    Thread.sleep(2000)
  }
}

//Allows player to heal with available healing items
def heal():String = {

  if (inventory.contains("stimpack") == false) {
    println(Console.CYAN + "\n" + "You don't have anymore stimpacks" + Console.WHITE)
  } else if (inventory.contains("stimpack") == true) {
    health += 25

    if (health > maxHealth) {
      health = maxHealth
      val heal = maxHealth - health
      inventory((inventory.indexWhere(_ == "stimpack"))) = "-"
      Console.CYAN + s"\n$heal HEALTH RECOVERED" + Console.WHITE
    } else {
      inventory((inventory.indexWhere(_ == "stimpack"))) = "-"
      Console.CYAN + "\n25 HEALTH RECOVERED" + Console.WHITE
    }
  }
  ""
}

//Create user interface that presents the player with gameplay options
def menu():Unit = {
  println(Console.GREEN + "\n-------------------------- Pip-Boy 500 --------------------------\n" + Console.WHITE)
  println("-------- Please type the option in \"" + Console.YELLOW + "YELLOW" + Console.WHITE + "\" to access -----------\n")
  println(">" + Console.YELLOW + " view" + Console.WHITE + " inventory")
  println(">" + Console.YELLOW + " stats" + Console.WHITE + " view")
  println(">" + Console.YELLOW + " equip" + Console.WHITE + " a weapon")
  println(">" + Console.YELLOW + " fasten" + Console.WHITE + " armor")
  println(">" + Console.YELLOW + " drop" + Console.WHITE + " an item")
  println(">" + Console.YELLOW + " use" + Console.WHITE + " a stimpack")
  println(">" + Console.YELLOW + " exit" + Console.WHITE + " inventory" + "\n")
  var option = readLine

  val weaponFilter = inventory.filter(inventory => inventory.contains("10") || inventory.contains("rifle") || inventory.contains("gun")).mkString("\n")
  val armorFilter = inventory.filter(inventory => inventory.contains("armor") || inventory.contains("suit")).mkString("\n")

  if (option == "view") {
    println("\n" + Console.GREEN + "---------- Inventory ----------" + Console.WHITE + "\n")
    println(s"Holster: $handEq")
    Thread.sleep(3000)
    println(s"Body: $armor\n")
    Thread.sleep(3000)
    println("Backpack:")
    println(inventory.mkString("\n"))
    print("\n")
    Thread.sleep(4000)
    menu()
  } else if (option == "stats") {

    viewSpec()
    print("\n")
    menu()

  } else if (option == "equip") {
    var empty = handEq

    println(Console.GREEN + "\n---------- Equipping Weapon ----------" + Console.WHITE)
    println("\nWhat weapon would you like to equip?")
    println(Console.YELLOW + weaponFilter + Console.WHITE)
    print("\n")
    handEq = readLine

    if (weaponFilter.contains(handEq) == true) {
      inventory((inventory.indexWhere(_ == handEq))) = "-"
      inventory((inventory.indexOf("-"))) = empty
      println("\n" + Console.CYAN + handEq.toUpperCase + " EQUIPPED" + "\n" + Console.WHITE)
      Thread.sleep(2000)
    } else if (weaponFilter.contains(handEq) == false) {
      println(Console.CYAN + s"\n$protagName: Can't defend myself with something like that...\n" + Console.WHITE)
      handEq = empty
      Thread.sleep(2000)
    }

    menu()

  } else if (option == "fasten") {
    var empty = armor

    println(Console.GREEN + "\n-------- Equipping Body Armor --------" + Console.WHITE)
    println("\nWhat armor would you like to equip?")
    println(Console.YELLOW + armorFilter + Console.WHITE)
    print("\n")
    armor = readLine

    if (armorFilter.contains(armor) == true) {
      protec()
      inventory((inventory.indexWhere(_ == armor))) = "-"
      inventory((inventory.indexOf("-"))) = empty
      println("\n" + Console.CYAN + armor.toUpperCase + " EQUIPPED" + "\n" + Console.WHITE)
      Thread.sleep(2000)
    } else if (armorFilter.contains(armor) == false) {
      println(Console.CYAN + s"\n$protagName: That's not body armor...\n" + Console.WHITE)
      armor = empty
      Thread.sleep(2000)
    }

    menu()

  } else if (option == "drop") {

    println(Console.GREEN + "\n---------- Discarding ---------" + Console.WHITE)
    print("\n")
    drop()
    print("\n")
    Thread.sleep(2000)

  } else if (option == "use") {

    if (inventory.contains("stimpack") == true) {
      if (health == maxHealth) {
        println(Console.CYAN + s"\n$protagName: It'd be a waste to use something I don't need...\n" + Console.WHITE)
        Thread.sleep(2000)
      } else {
        print(heal())
        println("\n")
        Thread.sleep(2000)
      }

    } else if (inventory.contains("stimpack") == false) {
      println(s"\n$protagName: The radiation must be getting to me...\n")
      Thread.sleep(2000)
    }
    menu()

  } else if (option == "exit") {
    println(Console.GREEN + "\n------------------ You put away your Pip-Boy --------------------" + Console.WHITE)
  } else {
    println(Console.CYAN + s"\n$protagName: I don't think my Pip-Boy has that option...\n" + Console.WHITE)
    Thread.sleep(2000)
    menu()
  }
}

//START OF GAME

println("\nSecurity Guard: Well, well, well. Look who it is...\n")

println("(Choose a name.)\n")
val protagName = readLine

println(s"\nSecurity Guard: It's $protagName. Looking to leave this place? I know I am, but the wife is unsure about it.")
Thread.sleep(7000)
println("Security Guard: Oh well. What can you do? Haha!")
Thread.sleep(5000)
println("Security Guard: Anyways, if you're looking to leave the vault, I need you to fill out this form. Ya' know, formalities and record-keeping and such.")
Thread.sleep(8000)
println("Security Guard: Answer the questions as follows, yeah?\n")
Thread.sleep(5000)

var specialPoints = 5

println(s"$protagName: (Oh! It's a S.P.E.C.I.A.L form. I remember Mom filling one out for us.)")
Thread.sleep(6000)
println(s"$protagName: (I've grown, though. Things change. I have to fill out a new one.)\n")
Thread.sleep(5000)
println("Security Guard: You can fiddle with the numbers if you'd like. Subtract one and add it to another.")
Thread.sleep(7000)
println("Security Guard: Write out the attribute and a number and I'll make sure to catalog it.")
Thread.sleep(7000)
println("Security Guard: Just make sure you write them on SEPARATE LINES, though.")
Thread.sleep(7000)
println("Security Guard: If not, you'd have to get a new form.")
Thread.sleep(5000)
println("Security Guard: Nobody wants that, right?\n")
Thread.sleep(5000)
println("Choose wisely.\n")

var inventory = Array("10mm pistol","vault armor", "stimpack","stimpack","-","-","-","-")
var handEq = "-"
var armor = "vault suit"
var localItem = ("")
var localNpc = ("")
var gameOver = false

var strength = 5
var perception = 5
var endurance = 5
var charisma = 5
var intelligence = 5
var agility = 5
var luck = 5

var done = false

do {
  while (specialPoints > 0) {
    println(s"You have $specialPoints points left\n")

    println(s"> strength: $strength")
    Thread.sleep(100)
    println(s"> perception: $perception")
    Thread.sleep(100)
    println(s"> endurance: $endurance")
    Thread.sleep(100)
    println(s"> charisma: $charisma")
    Thread.sleep(100)
    println(s"> intelligence: $intelligence")
    Thread.sleep(100)
    println(s"> agility: $agility")
    Thread.sleep(100)
    println(s"> luck: $luck\n")
    Thread.sleep(100)

    print("Attribute: ")
    var attribute = readLine
    print("Number: ")
    var points = readInt
    var choosePoint = (attribute, points)
    print("\n")


    while (choosePoint._2 > specialPoints) {
      println(s"$protagName: Uhh... It doesn't say I have that much on the form...\n")
      Thread.sleep(3000)
      println(s"You have $specialPoints points left\n")
      Thread.sleep(2000)
      println(s"> strength: $strength")
      println(s"> pecerption: $perception")
      println(s"> endurance: $endurance")
      println(s"> charisma: $charisma")
      println(s"> intelligence: $intelligence")
      println(s"> agility: $agility")
      println(s"> luck: $luck\n")

      print("Attribute: ")
      attribute = readLine
      print("Number: ")
      points = readInt
      choosePoint = (attribute, points)
      print("\n")
    }

    choosePoint match {
      case ("strength", x)     => strength += x
      case ("perception", x)   => perception += x
      case ("endurance", x)    => endurance += x
      case ("charisma", x)     => charisma += x
      case ("intelligence", x) => intelligence += x
      case ("agility", x)      => agility += x
      case ("luck", x)         => luck += x
      case _ =>
      println(s"\n$protagName: I gotta make sure to spell it right...\n")
      specialPoints += choosePoint._2
    }

    specialPoints -= choosePoint._2

    if (strength <= 1 ) {
      val difference = 1 - strength
      strength = 1
      specialPoints -= difference
    } else if (perception <= 0) {
        val difference = 1 - perception
        perception = 1
        specialPoints -= difference
    } else if (endurance <= 0) {
        val difference = 1 - endurance
        endurance = 1
        specialPoints -= difference
    } else if (charisma <= 0) {
        val difference = 1 - charisma
        charisma = 1
        specialPoints -= difference
    } else if (intelligence <= 0) {
        val difference = 1 - intelligence
        intelligence = 1
        specialPoints -= difference
    } else if (agility <= 0) {
        val difference = 1 - agility
        agility = 1
        specialPoints -= difference
    } else if (luck <= 0) {
        val difference = 1 - luck
        luck = 1
        specialPoints -= difference
    }

  }

  println(s"\n> strength: $strength")
  Thread.sleep(100)
  println(s"> perception: $perception")
  Thread.sleep(100)
  println(s"> endurance: $endurance")
  Thread.sleep(100)
  println(s"> charisma: $charisma")
  Thread.sleep(100)
  println(s"> intelligence: $intelligence")
  Thread.sleep(100)
  println(s"> agility: $agility")
  Thread.sleep(100)
  println(s"> luck: $luck")
  Thread.sleep(100)

  println("\nAre you done filling out the from?")
  Thread.sleep(2000)
  println("1. yes")
  println("2. no\n")
  println("Enter a number.\n")
  var answer = readInt

  while (answer != 1 && answer != 2) {
    wrongAnswer()
    println("1. yes")
    println("2. no\n")
    println("Enter a number.\n")
    answer = readInt
  }

  if (answer == 1) {
    done = true
  } else if (answer == 2) {
    println(s"\n$protagName: Crap. May I have another form?\n")
    Thread.sleep(2000)
    println("Security Guard: Sure thing. Here you go.\n")
    Thread.sleep(2000)

    done = false

    specialPoints = 5
    strength = 5
    perception = 5
    endurance = 5
    charisma = 5
    intelligence = 5
    agility = 5
    luck = 5
  }

} while (!done)

val maxHealth = hitPoints(endurance, luck)
var health = hitPoints(endurance, luck)
var attack = damage(strength, endurance, intelligence, luck)
var defense = block(endurance, agility, perception, luck)

println(s"\n$protagName: Done. Here you go.\n")
Thread.sleep(3000)
println("Security Guard: Great. Let me catalog your form and input the data into your Pip-Boy.")
Thread.sleep(5000)
println("Security Guard: Be back in a bit.")
Thread.sleep(1000)
println("...")
Thread.sleep(1000)
println("...")
Thread.sleep(1000)
println("...")
Thread.sleep(1000)
println("...")
Thread.sleep(1000)
println("...")
Thread.sleep(2000)
println("Security Guard: Alright. Here you go. Usually vaults aren't so lax about letting people out.")
Thread.sleep(6000)
println("Security Guard: Buuut Mount Desert is pretty safe compared to the rest of the U.S.")
Thread.sleep(5000)
println("Security Guard: Or at least I hope...")
Thread.sleep(3000)
println("Security Guard: Anyways, you be careful. You never know what might pop up.\n")
Thread.sleep(5000)
println(s"$protagName: Thank you, Sir. I will.\n")
Thread.sleep(2000)

println("Would you like to view your stats?")
Thread.sleep(2000)
println("1. yes")
println("2. no\n")
println("Enter a number.\n")
var viewStats = readInt

while (viewStats != 1 && viewStats != 2) {
  wrongAnswer()
  Thread.sleep(3000)
  println("Would you like to view your stats?")
  println("1. yes")
  println("2. no\n")
  println("Enter a number.\n")
  viewStats = readInt
}

if (viewStats == 1) viewSpec()

println("\nSecurity Guard: Before you go, look into that locker over there. You'll need the things inside.\n")
Thread.sleep(5000)
println(s"$protagName: Oh! Thanks.\n")
Thread.sleep(2000)
println("You open the locker and find a backpack, a holster, vault armor, a 10mm pistol, and a couple of stimpacks.\n")
Thread.sleep(7000)
println(s"$protagName: (This looks really useful. I should equip this gun.)\n")
Thread.sleep(3000)

println("Equip items?")
Thread.sleep(2000)
println("1. yes")
println("2. no\n")
println("Enter a number.\n")
var equip1 = readInt

while (equip1 != 1 && equip1 != 2) {
  wrongAnswer()
  println("1. yes")
  println("2. no\n")
  println("Enter a number.\n")
  equip1 = readInt
}

if (equip1 == 1) {
  println("\nYou've acquired a backpack and can now hold 8 items!")
  Thread.sleep(3000)
  println("You've acquired a holster which allows you to hold one weapon at a time!\n")
  Thread.sleep(4000)
  menu()
} else if (equip1 == 2) {
  println("\nYou've acquired a backpack and can now hold 8 items!")
  Thread.sleep(3000)
  println("You've acquired a holster which allows you to hold one weapon at a time!\n")
  Thread.sleep(4000)
}

println(s"\n$protagName: (Great. I'm ready to go)\n")
Thread.sleep(2000)

println(Console.CYAN + "Press \"enter\" to continue..." + Console.WHITE)
var continue = readLine

//LEAVING FOR BLACKWOODS
println("You exit the vault.\n")
Thread.sleep(3000)
println(s"$protagName: Jeez, that was noisy. *Sigh Contently* Nothing like a good hazy morning to start the day.")
Thread.sleep(5000)
println(s"$protagName: I need to find Dad. He's probably not on the island. I need to get to the airport.\n")
Thread.sleep(7000)
println("You make your way through what remains of civilization and eventually reach Route 3.")
Thread.sleep(5000)
println("But looking west, you see a thick fog rolling in on the island.\n")
Thread.sleep(5000)
println(s"$protagName: That doesn't look safe. It's a good thing I know a way around.\n")
Thread.sleep(5000)
println("You move east and follow the coast.\n")
Thread.sleep(3000)

println(Console.CYAN + "Press \"enter\" to continue..." + Console.WHITE)
continue = readLine

//AT BLACKWOODS
println("As you follow the road, you seem to reach the remains of BlackWoods Campground.\n")
Thread.sleep(7000)
println(s"$protagName: (Wow, they weren't kidding when they said the bombs wrecked everything.)")
Thread.sleep(5000)
println(s"$protagName: (The trailers seem to be intact.)\n")
Thread.sleep(3000)
println("What do you want to do?")
Thread.sleep(3000)
println("1. Let's see what we can find!")
println("2. I shouldn't waste time...")
println("3. Access inventory\n")
println("Enter a number.\n")
var search1 = readInt

while (search1 != 1 && search1 != 2 && search1 != 3) {
  wrongAnswer()
  Thread.sleep(3000)
  println("What do you want to do?")
  println("1. Let's see what we can find!")
  println("2. I shouldn't waste time...")
  println("3. Access inventory\n")
  search1 = readInt
}

if (search1 == 3) {
  do {
    menu()
    println("\nWhat do you want to do?")
    println("1. Let's see what we can find!")
    println("2. I shouldn't waste time...")
    println("3. Access inventory\n")
    search1 = readInt

    while (search1 != 1 && search1 != 2 && search1 != 3) {
      wrongAnswer()
      Thread.sleep(3000)
      println("What do you want to do?")
      println("1. Let's see what we can find!")
      println("2. I shouldn't waste time...")
      println("3. Access inventory\n")
      search1 = readInt
    }
  } while (search1 == 3)
}

if (search1 == 1) {
  println("\nYou enter the campground and find a few adandoned mobile homes.")
  Thread.sleep(5000)
  println("You approach one and open the door.")
  Thread.sleep(5000)
  println("You hear swift scurrying.\n")
  Thread.sleep(3000)
  println(s"$protagName: Shit! What the hell is that!?")
  Thread.sleep(3000)

  localNpc = "radroach"
  battle()
}

//OTTER CREEK
println("\nWith the campgrounds behind you, you make your way to the town of Otter Creek.")
Thread.sleep(5000)
println("Or at least, what's left of it...")
Thread.sleep(3000)
println("You make it to the center of town.\n")
Thread.sleep(5000)
println(s"$protagName: (Is there anyone left...?)\n")
Thread.sleep(4000)
println("As you pass the town hall, your question is answered by a crack from a bullet.\n")
Thread.sleep(7000)
println(s"$protagName: Fuck! What the hell!? I gotta get out of here!\n")
Thread.sleep(7000)
println("???: I know you're here! Come out wherever you are! I don't bite!\n")
Thread.sleep(5000)
println(s"$protagName: Here we go...")
Thread.sleep(4000)

localNpc = "bandit"
battle()

//DEFEATED BANDIT AND UNARMED MAN
println(s"\n$protagName: You had me dead to rights and you still messed up.")
Thread.sleep(4000)
println(s"$protagName: Though, I should probably feel remorseful that I killed another human being for the first time...")
Thread.sleep(8000)
println(s"$protagName: Psh! Naaah!\n")
Thread.sleep(3000)
println("With the bandit dispatched, you inspect his gear.")
Thread.sleep(4000)
println("...")
Thread.sleep(2000)
println("...")
Thread.sleep(2000)
println(s"$protagName: Nice! A combat rifle. Only the vault guards get these bad boys.")
Thread.sleep(5000)
println(s"$protagName: Not in the best condition, but plenty powerful.\n")
Thread.sleep(5000)
println("You sling the rifle and pocket the ammo. You continue on.\n")

localItem = "combat rifle"
println(Console.CYAN + localItem.toUpperCase + " FOUND" + Console.WHITE)
Thread.sleep(3000)

if (inventory.contains("-") == false) {
  pickUp(inventory, localItem)
  println(s"Would you like to make room for the $localItem?")
  Thread.sleep(3000)
  println("1. yes")
  println("2. no\n")
  println("Enter a number.\n")
  var drop0 = readInt

  while (drop0 != 1 && drop0 != 2) {
    wrongAnswer()
    Thread.sleep(3000)
    println(s"Would you like to make room for the $localItem?")
    println("1. yes")
    println("2. no\n")
    println("Enter a number.\n")
    drop0 = readInt
  }

  if (drop0 == 1) {
    drop()
    pickUp(inventory, localItem)
  }
} else {
  pickUp(inventory, localItem)
  println(s"Would you like to equip the $localItem?")
  Thread.sleep(3000)
  println("1. yes")
  println("2. no\n")
  println("Enter a number.\n")
  var equip0 = readInt

  while (equip0 != 1 && equip0 != 2) {
    wrongAnswer()
    Thread.sleep(3000)
    println(s"Would you like to equip the $localItem?")
    println("1. yes")
    println("2. no\n")
    println("Enter a number.\n")
    equip0 = readInt
  }

  if (equip0 == 1) menu()
}

println(Console.CYAN + "\nPress \"enter\" to continue..." + Console.WHITE)
continue = readLine

println("As you walk along the road, you come across an Unarmed Man.")
Thread.sleep(5000)
println("He is badly beaten, but none of the injuries are life threatening.\n")
Thread.sleep(5000)
println("Unarmed Man: You kill him? With all of that shooting, you better have!\n")
Thread.sleep(7000)
println(s"$protagName: Uhh...\n")
Thread.sleep(2000)
println("Unarmed Man: Wait... That's my rifle. You did kill him. Ha! That's karma for ya.")
Thread.sleep(7000)
println("Unarmed Man: You best get goin'. Those shots prolly attracted more of 'em.\n")
Thread.sleep(7000)
println(s"$protagName: (This man's in pretty bad shape. On top of that, I have his rifle. He might need it more than I do...)\n")
Thread.sleep(7000)
println("What do you want to do?")
Thread.sleep(3000)
println("1. Return rifle")
println("2. Leave")
println("3. Access inventory\n")
println("Enter a number.\n")
var choice1 = readInt

while (choice1 != 1 && choice1 != 2 && choice1 != 3) {
  println("\nUnarmed Man: What was that?\n")
  Thread.sleep(3000)
  println("1. Return rifle")
  println("2. Leave")
  println("3. Access inventory\n")
  println("Enter a number.\n")
  choice1 = readInt
}

if (choice1 == 3) {
  do {
    menu()
    println("\nWhat do you want to do?")
    Thread.sleep(3000)
    println("1. Return rifle")
    println("2. Leave")
    println("3. Access inventory\n")
    println("Enter a number.\n")
    choice1 = readInt

    while (choice1 != 1 && choice1 != 2 && choice1 != 3) {
      println("\nUnarmed Man: What was that?\n")
      Thread.sleep(3000)
      println("What do you want to do?")
      println("1. Return rifle")
      println("2. Leave")
      println("3. Access inventory\n")
      println("Enter a number.\n")
      choice1 = readInt
    }
  } while (choice1 == 3)

}

if (choice1 == 1) {
  if (inventory.contains("combat rifle") == true) {
    inventory((inventory.indexWhere(_ == "combat rifle"))) = "-"
    println("\n" + Console.CYAN + "COMBAT RIFLE REMOVED" + Console.WHITE)
  } else if (handEq == "combat rifle") {
    handEq = "-"
    println("\n" + Console.CYAN + "COMBAT RIFLE REMOVED" + Console.WHITE)
  }

  Thread.sleep(3000)
  println("\nUnarmed Man: Re... Really? I figured you'd keep it. Thank you!")
  Thread.sleep(5000)
  println("Unarmed Man: Wait. Before ya' go, let me give you somethin' in return.")
  Thread.sleep(5000)
  println("Unarmed Man: I set up camp in the southern most warehouse at Jackson Laboratory.")
  Thread.sleep(6000)
  println("Unarmed Man: The code is 065. Help yourself to what I got. It's the least I can do for ya'.\n")
  Thread.sleep(7000)
  println(s"$protagName: Thank you. Good luck.\n")
  Thread.sleep(4000)

  localItem = "jlc:065"
  pickUp(inventory, localItem)

} else if (choice1 == 2) {
  println(s"\n$protagName: Thanks. I will. See ya'.")
  Thread.sleep(3000)
  println(s"$protagName:(Nice weapon. Good thing he didn't want it back. He'd probably lose it again.)\n")
}

println(Console.CYAN + "Press \"enter\" to continue..." + Console.WHITE)
continue = readLine

//JACKSON LABORATORY
println("You spend the next hour walking north.")
Thread.sleep(5000)
println("You approach Jackson Laboratory. A genomic research facility used for weapons testing during the war.")
Thread.sleep(7000)
println("The buildings in the complex show clear signs of looting and vandalism. The location is a shell of what it once was.")
Thread.sleep(8000)
println("You approach a somewhat preserved warehouse with a code lock on it.\n")
Thread.sleep(6000)
println("What do you do?")
Thread.sleep(2000)
println("1. Enter code")
println("2. Do nothing and leave\n")
println("Enter a number.\n")
var choice3 = readInt

while (choice3 != 1 && choice3 != 2) {
  wrongAnswer()
  Thread.sleep(3000)
  println("What do you do?")
  println("1. Enter code")
  println("2. Do nothing and leave\n")
  println("Enter a number.\n")
  choice3 = readInt
}

if (choice3 == 1) {
  if (inventory.contains("jlc:065") == true) {
    println(s"\n$protagName: Better than nothing.")
    Thread.sleep(2000)
    println("\nYou find a stash of supplies.")
    Thread.sleep(2000)
    println("You help yourself to some stimpacks.\n")
    Thread.sleep(3000)

    localItem = "stimpack"
    println(Console.CYAN + localItem.toUpperCase + "S" + " FOUND" + Console.WHITE + "\n")
    Thread.sleep(3000)
    var quantity = 2

    while (quantity > 0) {
      if (inventory.contains("-") == false) {
        pickUp(inventory, localItem)
        println(s"Would you like to make room for the $localItem?")
        Thread.sleep(3000)
        println("1. yes")
        println("2. no\n")
        println("Enter a number.\n")
        var drop1 = readInt

        while (drop1 != 1 && drop1 != 2) {
          wrongAnswer()
          Thread.sleep(3000)
          println(s"Would you like to make room for the $localItem?")
          println("1. yes")
          println("2. no\n")
          println("Enter a number.\n")
          drop1 = readInt
        }

        if (drop1 == 1) {
          drop()
          pickUp(inventory, localItem)
        }
        quantity -= 1
      } else {
        pickUp(inventory, localItem)
        Thread.sleep(2000)
        quantity -= 1
      }
    }
  } else if (inventory.contains("jlc:065") == false) {
      println(s"\n$protagName: There's no way I'm getting passed this lock without the code. Oh well...\n")
      Thread.sleep(5000)
      println("You leave the complex and continue north.")
  }
} else if (choice3 == 2) {
  println(s"\n$protagName: It'd be a waste of time.\n")
  Thread.sleep(3000)
  println("You leave the complex and continue north.")
}


println(Console.CYAN + "Press \"enter\" to continue..." + Console.WHITE)
continue = readLine

//BAR HARBOR
println("You enter the town of Bar Harbor.\n")
Thread.sleep(4000)
println(s"$protagName: This place is pretty occupied...\n")
Thread.sleep(4000)
println("A group of survivors started turning Town Pier into a fortress. \nThey have also started labeling the place as \"Far Harbor.\"")
Thread.sleep(8000)
println("After some observation, you decide it's best to keep your distance.\n")
Thread.sleep(8000)
println(s"$protagName: (While I'm here, maybe I can find a boat. That way I don't have to walk all the way to the bridge.)")
Thread.sleep(9000)
println(s"$protagName: (I'm sure I'm less likely to get shot too.)\n")
Thread.sleep(5000)
println("1. It's worth a shot. Look for a boat")
println("2. Most of the boats are either destroyed or washed out to sea. There's no point.")
println("3. Access inventory\n")
println("Enter a number.\n")
var barharChoice = readInt

while (barharChoice != 1 && barharChoice != 2 && barharChoice != 3) {
  wrongAnswer()
  Thread.sleep(4000)
  println("1. It's worth a shot. Look for a boat")
  println("2. Most of the boats are either destroyed or washed out to sea. There's no point.")
  println("3. Access inventory\n")
  println("Enter a number.\n")
  barharChoice = readInt
}

if (barharChoice == 3) {
  do {
    menu()
    println("\nWhat do you want to do?")
    println("1. It's worth a shot. Look for a boat")
    println("2. Most of the boats are either destroyed or washed out to sea. There's no point.")
    println("3. Access inventory\n")
    println("Enter a number.\n")
    barharChoice = readInt

    while (barharChoice != 1 && barharChoice != 2 && barharChoice != 3) {
      wrongAnswer()
      Thread.sleep(3000)
      println("What do you want to do?")
      println("1. It's worth a shot. Look for a boat")
      println("2. Most of the boats are either destroyed or washed out to sea. There's no point.")
      println("3. Access inventory\n")
      println("Enter a number.\n")
      barharChoice = readInt
    }
  } while (barharChoice == 3)

}

if (barharChoice == 1) {
  println("\nYou search the outskirts of the town. You eventually enter an intact boathouse.\n")
  Thread.sleep(7000)
  println(s"$protagName: (No boat here, but there is definetly something in the water...)")

  localNpc = "mirelurk"
  battle()

  println(s"\n$protagName: What an ugly thing. *Sigh* There's no boat, but there is a boat propeller.")
  Thread.sleep(5000)
  println(Console.CYAN + localItem.toUpperCase + " FOUND" + Console.WHITE)
  Thread.sleep(5000)
  println("\nWhat do you want to do?")
  Thread.sleep(3000)
  println("1. It's the only undamaged one I have found. I'll keep it.")
  println("2. No boat, no use. Leave it.\n")
  println("Enter a number.\n")
  var propChoice = readInt

  while (propChoice != 1 && propChoice != 2) {
    wrongAnswer()
    Thread.sleep(3000)
    println("What do you want to do?")
    println("1. It's the only undamaged one I have found. I'll keep it.")
    println("2. No boat, no use. Leave it.")
    println("Enter a number.\n")
    propChoice = readInt
  }

  if (propChoice == 1) {
    println("\nFor some reason, you decide to keep the chunk of aluminum and afix it to your pack.")
    Thread.sleep(6000)
    println("...")
    Thread.sleep(2000)
    println("...")
    Thread.sleep(2000)
    println(s"$protagName: (This isn't heavy at all. Almost as if whatever I take is weightless.)\n")
    Thread.sleep(6000)

    localItem = "propeller"
    Thread.sleep(3000)

    if (inventory.contains("-") == false) {
      pickUp(inventory, localItem)
      println(s"Would you like to make room for the $localItem?")
      Thread.sleep(3000)
      println("1. yes")
      println("2. no\n")
      println("Enter a number.\n")
      var drop2 = readInt

      while (drop2 != 1 && drop2 != 2) {
        wrongAnswer()
        Thread.sleep(3000)
        println(s"Would you like to make room for the $localItem?")
        println("1. yes")
        println("2. no\n")
        println("Enter a number.\n")
        drop2 = readInt
      }

      if (drop2 == 1) {
        drop()
        pickUp(inventory, localItem)
      }
    } else {
      pickUp(inventory, localItem)
    }
  }
}

println("\nYou hit the road and put Bar Harbor behind you.\n")

println(Console.CYAN + "Press \"enter\" to continue..." + Console.WHITE)
continue = readLine

//HULLS COVE
println("As the sun sets, you take refuge in an abandoned house.")
Thread.sleep(4000)
println("By sunrise, you are awake and heading up the coast.")
Thread.sleep(4000)
println("It isn't long before you reach Hulls Cove.\n")
Thread.sleep(4000)
println("What do you do?")
Thread.sleep(3000)
println("1. Look for a boat")
println("2. The residents may not be friendly. Best to hurry through.")
println("3. Access inventory\n")
println("Enter a number.\n")
var hullsChoice = readInt

while (hullsChoice != 1 && hullsChoice != 2 && hullsChoice != 3) {
  wrongAnswer()
  Thread.sleep(3000)
  println("\nWhat do you do?")
  println("1. Look for a boat")
  println("2. The residents may not be friendly. Best to hurry through.")
  println("3. Access inventory\n")
  println("Enter a number.\n")
  hullsChoice = readInt
}

if (hullsChoice == 3) {
  do {
    menu()
    println("\nWhat do you do?")
    println("1. Look for a boat")
    println("2. The residents may not be friendly. Best to hurry through.")
    println("3. Access inventory\n")
    println("Enter a number.\n")
    hullsChoice = readInt

    while (hullsChoice != 1 && hullsChoice != 2 && hullsChoice != 3) {
      wrongAnswer()
      Thread.sleep(3000)
      println("What do you do?")
      println("1. Look for a boat")
      println("2. The residents may not be friendly. Best to hurry through.")
      println("3. Access inventory\n")
      println("Enter a number.\n")
      hullsChoice = readInt
    }
  } while (hullsChoice == 3)

}

if (hullsChoice == 1) {
  println("\nYou search piers and boathouses in hopes of finding a worthy sea vessel.\n")
  Thread.sleep(5000)
  println(s"$protagName: (No boats here...)\n")
  Thread.sleep(4000)
  println("Since you're here, you decide to search for supplies.")
  Thread.sleep(4000)
  println("1. There's a nearby Public Works station.")
  println("2. The nearby Retirement Village might have medical supplies.")
  println("3. Access inventory\n")
  println("Enter a number.\n")
  var hullsChoice2 = readInt

  if (hullsChoice2 == 1) {
    println("\nYou arrive at the Public Works station.")
    Thread.sleep(4000)
    println("You enter the main office. The building is filled with dusty and old papers.")
    Thread.sleep(5000)
    println("...")
    Thread.sleep(2000)
    println("...\n")
    Thread.sleep(2000)
    println(s"$protagName: (There's nothing here. What a waste...)\n")
    Thread.sleep(4000)
    println("Before you exit, you bend down to tie your shoe. As you bend down, you see a skeleton under a nearby desk.")
    Thread.sleep(8000)
    println("The skeleton is clutching onto a dusty shotgun. You decide to borrow it.\n")
    Thread.sleep(5000)
    println(s"$protagName: (Thanks friend. You won't be needing this anymore.)\n")
    Thread.sleep(7000)
    println("You pocket the loose shells and exit the office.\n")

    localItem = "shotgun"
    println(Console.CYAN + localItem.toUpperCase + " FOUND" + Console.WHITE)
    Thread.sleep(3000)

    if (inventory.contains("-") == false) {
      pickUp(inventory, localItem)
      println(s"Would you like to make room for the $localItem?")
      Thread.sleep(3000)
      println("1. yes")
      println("2. no\n")
      println("Enter a number.\n")
      var drop3 = readInt

      while (drop3 != 1 && drop3 != 2) {
        wrongAnswer()
        Thread.sleep(3000)
        println(s"Would you like to make room for the $localItem?")
        println("1. yes")
        println("2. no\n")
        println("Enter a number.\n")
        drop3 = readInt
      }

      if (drop3 == 1) {
        drop()
        pickUp(inventory, localItem)
      }
    } else {
      pickUp(inventory, localItem)
      println(s"Would you like to equip the $localItem?")
      Thread.sleep(3000)
      println("1. yes")
      println("2. no\n")
      println("Enter a number.\n")
      var equip3 = readInt

      if (equip3 == 1) menu()
    }

  } else if (hullsChoice2 == 2) {
    println("\nYou make your way to the Retirement Village.\n")
    Thread.sleep(4000)
    println(s"$protagName: (I'm going to assume the residents don't need their medical supplies anymore.)\n")
    Thread.sleep(6000)
    println("You come to the first home and enter. As you expected, the residents are corpses on the ground.")
    Thread.sleep(6000)
    println("What you didn't expect though was for the corpse to start moving.\n")
    Thread.sleep(5000)

    localNpc = "ghoul"
    battle()

    println("\nWith the shambler dropped, you quickly exit the house. Looking through the rest of the homes isn't worth it.")
    Thread.sleep(7000)
    println("You leave the village empty handed.\n")
    Thread.sleep(5000)
  }
}

println(Console.CYAN + "\nPress \"enter\" to continue..." + Console.WHITE)
continue = readLine

//LEAVING HULLS COVE
println("On the northeast side of town, you stop to plan your route.\n")
Thread.sleep(4000)
println(s"$protagName: (I can continue to follow the road and risk running into more bandits.)")
Thread.sleep(4000)
println(s"$protagName: (Or I can cut through the woods. But who knows what's in there.)\n")
Thread.sleep(4000)
println("What do you want to do?")
Thread.sleep(3000)
println("1. Stick to the road. The woods are spooky.")
println("2. Cut through the woods. It's much faster.")
println("3. Access inventory.\n")
println("Enter a number.\n")
var routeChoice = readInt

while (routeChoice != 1 && routeChoice != 2 && routeChoice != 3) {
  println(s"\n$protagName: If there was a different route, it would show up...\n")
  Thread.sleep(4000)
  println("\nWhat do you want to do?")
  println("1. Stick to the road. The woods are spooky.")
  println("2. Cut through the woods. It's much faster.")
  println("3. Access inventory.\n")
  println("Enter a number.\n")
  routeChoice = readInt
}

if (routeChoice == 3) {
  do {
    menu()
    println("\nWhat do you want to do?")
    println("1. Stick to the road. The woods are spooky.")
    println("2. Cut through the woods. It's much faster.")
    println("3. Access inventory.\n")
    println("Enter a number.\n")
    routeChoice = readInt

    while (routeChoice != 1 && routeChoice != 2 && routeChoice != 3) {
      wrongAnswer()
      Thread.sleep(4000)
      println("What do you want to do?")
      println("1. Stick to the road. The woods are spooky.")
      println("2. Cut through the woods. It's much faster.")
      println("3. Access inventory.\n")
      println("Enter a number.\n")
      routeChoice = readInt
    }
  } while (routeChoice == 3)

}

if (routeChoice == 1) {
  println("\nYou take the road all the way to the northern coast. You eventually reach Bar Harbor campgrounds\n")
  Thread.sleep(5000)
  println(s"$protagName: (That fog is still to the southwest. Hopefully the bridge is still accessible.)\n")
  Thread.sleep(5000)
  println("You enter the campgrounds. As you walk by some abandoned campers, you hear a growl.")
  Thread.sleep(5000)

  localNpc = "yao guai"
  battle()

  println("\nYou give Boo-Boo a casket instead of a basket. You search the campgrounds in peace.\n")
  Thread.sleep(7000)

  localItem = "metal armor"
  println(Console.CYAN + localItem.toUpperCase + " FOUND" + Console.WHITE)
  Thread.sleep(3000)

  if (inventory.contains("-") == false) {
    pickUp(inventory, localItem)
    println(s"Would you like to make room for the $localItem?")
    Thread.sleep(3000)
    println("1. yes")
    println("2. no\n")
    println("Enter a number.\n")
    var drop4 = readInt

    while (drop4 != 1 && drop4 != 2) {
      wrongAnswer()
      Thread.sleep(3000)
      println(s"Would you like to make room for the $localItem?")
      println("1. yes")
      println("2. no\n")
      println("Enter a number.\n")
      drop4 = readInt
    }

    if (drop4 == 1) {
      drop()
      pickUp(inventory, localItem)
    }
  } else {
    pickUp(inventory, localItem)
    println(s"Would you like to equip the $localItem?")
    Thread.sleep(3000)
    println("1. yes")
    println("2. no\n")
    println("Enter a number.\n")
    var equip4 = readInt

    if (equip4 == 1) menu()
  }

  println("\nYou leave the campground and continue west.\n")

} else if (routeChoice == 2) {
  println("\nYou enter the woods and plow your way through shrubbery and dense undergrowth.")
  Thread.sleep(5000)
  println("As you head west, you hear various animal calls. Each more foreign than the last. You move quickly, but cautiously.\n")
  Thread.sleep(7000)
  println(s"$protagName: (I guess I shouldn't be surprised. Who knows how much radiation has changed things.)\n")
  Thread.sleep(7000)
  println("You eventually come to a clearing besides a running brook. You would have stopped to rest, but you're not alone.\n")
  Thread.sleep(7000)

  localNpc = "yao guai"
  battle()

  println(s"\n$protagName: (Sorry Yogi, no picnic basket for you.)\n")
  Thread.sleep(3000)
  println("Out of fear of encountering more animals, you crash through the vegetation in a shower of broken twigs and flying leaves.")
  Thread.sleep(7000)
  println("You come to a road at the wood's edge. Across the street from you is a marine service center.")
  Thread.sleep(5000)
  println("Feeling that something is still stalking you in the trees, you seek shelter in the store. It is a far more defensible position.")
  Thread.sleep(8000)
  println("Inside, you find dozens of undamaged boats. However, unless you plan on dragging them to the coast, they are useless to you.")
  Thread.sleep(8000)
  println("\nWhat do you want to do?")
  Thread.sleep(3000)
  println("1. Salvage engine parts.")
  println("2. I'll take the road north.\n")
  println("Enter a number.\n")
  var partsChoice = readInt

  while (partsChoice != 1 && partsChoice != 2) {
    wrongAnswer()
    println("What do you want to do?")
    println("1. Salvage engine parts.")
    println("2. I'll take the road north.\n")
    println("Enter a number.\n")
    partsChoice = readInt
  }

  if (partsChoice == 1) {
    if (inventory.contains("-") == false) {
      println("\nIt takes some time, but you manage to disassemble a few engines.\n")
      Thread.sleep(4000)
      println("\nYou take spark plugs, gaskets, hoses, and any other engine part that would usually break.\n")
      Thread.sleep(6000)
      println("But...")
      Thread.sleep(2000)
      pickUp(inventory, localItem)
      Thread.sleep(3000)
      println(s"Would you like to make room for the $localItem?")
      Thread.sleep(3000)
      println("1. yes")
      println("2. no\n")
      println("Enter a number.\n")
      var drop5 = readInt

      while (drop5 != 1 && drop5 != 2) {
        wrongAnswer()
        Thread.sleep(3000)
        println(s"Would you like to make room for the $localItem?")
        println("1. yes")
        println("2. no\n")
        println("Enter a number.\n")
        drop5 = readInt
      }

      if (drop5 == 1) {
        drop()
        pickUp(inventory, localItem)
      }
    } else {
      println("\nIt takes some time, but you manage to disassemble a few engines.\n")
      Thread.sleep(4000)
      println("\nYou take spark plugs, gaskets, hoses, and any other engine part that would usually break.\n")
      Thread.sleep(6000)
      localItem = "bundle of parts"
      pickUp(inventory, localItem)
      Thread.sleep(3000)
    }
  }
}

println(Console.CYAN + "\nPress \"enter\" to continue..." + Console.WHITE)
continue = readLine

//TOWN OF SALISBURY
println("By some miracle, you make it to salisbury Cove in one piece.")
Thread.sleep(5000)
println("The Trenton Bridge is closer now, and the fog should still be to the southwest.\n")
Thread.sleep(5000)
println("1. There are some large buildings still standing. Worth taking a look.")
println("2. I've come so far. No stopping now.")
println("3. Access inventory.\n")
println("Enter a number.\n")
var salisburyChoice = readInt

while (salisburyChoice != 1 && salisburyChoice != 2 && salisburyChoice != 3) {
  wrongAnswer()
  Thread.sleep(3000)
  println("What do you want to do?")
  println("1. There are some large buildings still standing. Worth taking a look.")
  println("2. I've come so far. No stopping now.")
  println("3. Access inventory.\n")
  println("Enter a number.\n")
  salisburyChoice = readInt
}

if (salisburyChoice == 3) {
  do {
    menu()
    println("\nWhat do you want to do?")
    println("1. There are some large buildings still standing. Worth taking a look.")
    println("2. I've come so far. No stopping now.")
    println("3. Access inventory.\n")
    println("Enter a number.\n")
    salisburyChoice = readInt

    while (salisburyChoice != 1 && salisburyChoice != 2 && salisburyChoice != 3) {
      wrongAnswer()
      Thread.sleep(3000)
      println("What do you want to do?")
      println("1. There are some large buildings still standing. Worth taking a look.")
      println("2. I've come so far. No stopping now.")
      println("3. Access inventory.\n")
      println("Enter a number.\n")
      salisburyChoice = readInt
    }
  } while (salisburyChoice == 3)

}

if (salisburyChoice == 1) {
  println("\nYou go to the shore and find MDI Laboratory. Like the other lab, this place is looted.")
  Thread.sleep(5000)
  println("While moving through the buildings, you spot a small dock on the water. And attached to it, a small boat.")
  Thread.sleep(7000)
  println("You run to the boat and inspect it. The hull has holes and the furnishing is damaged, but it is still useable.")
  Thread.sleep(7000)
  println("You inspect the engine and find that the fuel tank is nearly full, but several other components are missing.\n")
  Thread.sleep(8000)
  println("\nWhat do you want to do?")
  Thread.sleep(3000)
  println("1. Try to fix the boat.")
  println("2. Head for the bridge.\n")
  println("Enter a number.\n")
  var boatChoice = readInt

  while (boatChoice != 1 && boatChoice != 2) {
    println(s"\n$protagName: Maybe if I dunk my head in the water, I won't answer with the wrong choice.\n")
    Thread.sleep(5000)
    println("\nWhat do you want to do?")
    println("1. Try to fix the boat.")
    println("2. Head for the bridge.")
    println("Enter a number.\n")
    boatChoice = readInt
  }

  if (boatChoice == 1) {
    if (inventory.contains("propeller") == true && inventory.contains("bundle of parts") == true) {
      println("\nWith the parts you have, you get to work on the engine. Once fixed, you try starting it up.")
      Thread.sleep(6000)
      println("...")
      Thread.sleep(2000)
      println("...")
      Thread.sleep(2000)
      println(s"$protagName: Yes! It worked!\n")
      Thread.sleep(4000)
      println(Console.CYAN + "You set sail across the Narrows and away from the Island." + Console.WHITE)
      System.exit(1)
    } else {
      println("\nYou lack the material necessary to fix the boat. You head for the bridge.\n")
    }
  }
}

println(Console.CYAN + "Press \"enter\" to continue..." + Console.WHITE)
continue = readLine

//BRIDGE
println("You continue down the road on the last leg of your adventure.\n")
Thread.sleep(5000)
println(s"$protagName: (What if the bridge is collapsed? Or maybe it's occupied?)\n")
Thread.sleep(7000)
println("You suppress these concerns and press on, eventually reaching your destination.")
Thread.sleep(7000)
println("You find that the bridge has collapsed, taking a buildup of cars with it on the way down into the sea.")
Thread.sleep(8000)
println("Undeterred, you figure that the frame of the structure is still sound enough to climb across.")
Thread.sleep(8000)
println("Suspended over the maelstrom of warped metal and destroyed cars, you proceed across.")
Thread.sleep(8000)
println("After a few close calls, you finally make it across.\nAs you plant your feet on solid ground, you are greeted with a roar.")
Thread.sleep(8000)

localNpc = "deathclaw"
battle()

Thread.sleep(5000)

println("\nThe Deathclaw is finally defeated. Successfully across the Narrows, you set off for your next destination.\n")
Thread.sleep(10000)
println(Console.MAGENTA + "To be continued...\n" + Console.WHITE)
