# CSE230-Group-46
Final Project for CSE230 - Alexander Yu, Arjun Sampath, Christopher Cha, Eric Xiao


We plan to develop a 2 player competitive online Wordle clone using a peer to peer network. Although there already exists a Wordle clone using the Brick library, we hope to implement enough new features specific to online multiplayer in order to differentiate our project. Our main inspiration for this project is Squabble (https://squabble.me/), a battle royal version of Wordle, however we deemed that networking for a large lobby of players would be too complicated for the scope of our project. Therefore, we will attempt to design a pseudo-turn-based version of the game while continuing to keep the game intense. 

During a game, one of two players will be chosen to make their first 5 letter guess, with the incorrect, correct but in the wrong location, and correct in the right location letters displayed. Once the guess has been made, the state will be sent to the other player, who will then make their guess and receive the same feedback. This will continue until either player selects the correct word. We also plan to implement a rudimentary timer for each player’s turn in order to keep the pace of the game going and introduce a stressor to the players. We hope to also include more features to impede the other player or benefit the user given various scenarios (powerups, combos) and other modes that would act as extensions to the main mode.
