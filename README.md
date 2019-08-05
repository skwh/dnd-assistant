# dnd-assistant

A command-line assistant for playing Dungeons and Dragons.

## Features

### Roll

```
> roll 3 5
2, 4, 1
> roll 3 5 +
7
```


# REVISION - REDUCED SCOPE - SEE ABOVE

## Features

### Roll

If the first roll is a critical hit or critial fail, you will be told.

```
> roll dex // roll a d20 + dexterity modifier
18
> roll intelligence // roll a d20 + intelligence modifier
nat 1
```
etc.

A list of usable stats (and, if avaliable, an alias) can be seen below.

- Strength (STR)
- Athletics
- Dexterity (DEX)
- Acrobatics
- Stealth
- SleightOfHand (SOH)
- Constitution (CON)
- Intelligence (INT)
- Arcana
- History
- Investigation
- Nature
- Religion
- Wisdom (WIS)
- AnimalHandling
- Insight
- Medicine
- Perception (PER)
- Survival
- Charisma (CHA)
- Deception
- Intimidation
- Performance
- Persuasion

### Checks vs saves

check = d20 + modifier
save = d20 + modifier + proficiency bonus

### Adding a character

```
> new
Please enter a name for your character.
> Dolomir
Please enter this character's stat for: Strength
> +3
...
Please enter this character's stat for: Initiative
> -1
Character saved as: Dolomir
```

### Updating a character's stats

```
> mod Strength +4
Dolomir's strength is now 4.
```

### Rolling a random die

```
> roll d20
4
> roll d8
1
> roll d100
84
> roll 3d8
5, 2, 6
> roll 2d4
2, 1
> roll 2d4 +2
6, 3
```

### Using a different character

```
> which
Dolomir
> list
Dolomir, Ingrid
> use Ingrid
Using character: Ingrid
```

### Adding to balance

```
> add 20gp
Ingrid's balance is now 20gp.
> add 10sp
Ingrid's balance is now 21gp. 
```

### Checking balance

```
> balance
Ingrid's balance is 21gp.
```

### Subtracting from balance

```
> sub 20gp
Ingrid's balance is now 1gp.
```

## List of commands
- roll NdM < + | -  K> < check | save > | STAT
- add J [ gp | sp | cp ]
- sub " "
- balance
- which
- list
- mod STAT [ + | - ] K
- new
- use <CHARACTER NAME>

Where N, M, K, and J are natural numbers.

## Architecture

### Lib.hs

The entry point for the program. Handles the REPL loop.

### DndFile.hs

Handles saving and loading character stats.

### Stats.hs

Handles stat storage.

### Dice.hs

Handles dice rolling.

## Other planned features

- HP Handling
- Spell slots
- Death save success and fail