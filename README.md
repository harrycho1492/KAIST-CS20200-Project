# KAIST-CS20200-Project
Term project repository for KAIST CS.20200 course

### Project title: CLI 3-person Japanese Mahjong

This project aims to implement a single round of 3-person Japanese Mahjong in command line. The user will be playing against two bots. The rules will follow that from the *Mahjong Soul* online game platform.

### Running the Project

Enter `dotnet run` to run the project. You will be greeted with the main menu, where you can select your position and initiate the game. All interactions with the game is done by entering a decimal number.

Disclaimer: although extensive effort was made to test the project, there may be specific untested circumstances with very low chances of occurence where the game does not behave as intended or where the program errorneously terminates. I apologize for the inconvenience in advance in case it happens.

### Rules

- 27 kinds of tiles are used in the game: 1 and 9 of Characters, 1 through 9 of Circles, 1 through 9 of Bamboos, and 7 Honors - East, South, West, North, White, Green, Red. There are 4 of each tile, which brings the total number of tiles to 108.
- Each tile is represented with its shorthand notation. `1m` and `9m` for Characters(*Man*), `1p` through `9p` for Circles(*Pin*), `1s` through `9s` for Bamboos(*So*), and `1z` through `7z` for Honors(*Zi*).
  - There are 2 red bonus tiles, `5P` and `5S`, which each gives you 1 bonus point if you win with this tile in your possession.
- Players 1, 2, 3 will each be designated the seats of East, South, and West. The turn is passed in this order.
- The 108 tiles are distributed as the following when the game starts.
  - Each player is given 13 starting tiles.
  - 55 tiles are stacked in the middle of the table, from which the players will draw tiles from.
  - 10 tiles are used as bonus display tiles: the tiles following the bonus display tiles gives you 1 bonus point if you win with it in your possession. (For example, if `2s` is the bonus display tile, then every `3s` tile will give you 1 bonus point. The tile following `1m`, `9m`, `9p`, `9s`, `4z`, `7z` are considered to be `9m`, `1m`, `1p`, `1s`, `1z`, `5z`, respectively.)
  - 4 tiles are put aside and not used, until a player makes a quad or puts aside a North (`4z`) tile. On such occurences, the player draws their next tile from this stack instead of the standard stack. One tile is picked from the standard stack to refill the now missing tile.
- The basic goal is to build 4 *Bodies* (groups of 3 tiles) and 1 *Head* (group of 2 tiles) with the tiles in your possession.
  - *Bodies*: Triplets (3 tiles of the same kind) / Sequences (3 tiles that are in sequence: for example, `2p`, `3p` and `4p`)
  - *Head*: Pair (2 tiles of the same kind)
  - Exceptions (1): *Seven Pairs* winning hand. Achieved when 7 *Heads* are built.
  - Exceptions (2): *Thirteen Orphans | 13-tile wait* and *Thirteen Orphans* limit hands. Achieved when 14 tiles of 13 specific kinds are gathered.
- Building tiles can be achieved through the following means.
  - On your turn: Drawing and discarding tiles.
    - Closed quad: Possible when you have 4 tiles of one kind in your hand. Quads are counted as 1 *Body*, are disclosed to all players when made, and cause a new bonus tile to be disclosed when made. After making a quad, you get the next turn.
    - Added quad: Possible when you have a called triplet made previously and have a tile of the same kind in your hand. See 'Closed quad' for shared characteristics.
    - Putting aside a North (`4z`) tile: Possible when you have any North (`4z`) tiles in your hand. These tiles are counted as bonus to your score when you win, and are disclosed to all players. After doing this move, you get the next turn.
  - On other players' turns: Calling for other players' discarded tiles when the conditions are met. Called tiles are disclosed to all players.
    - Win by discard: Possible when you are one tile away from completing a winning hand, and when the discarded tile is what is missing. The game instantly finishes with you being victorious. Win by discard can be called for added quads and North (`4z`) tiles put aside, too.
    - Open quad: Possible when you have 3 tiles of one kind in your hand, and when the discarded tile is of the same kind. See 'Closed quad' for shared characteristics.
    - Called triplet: Possible when you have 2 or more tiles of one kind in your hand, and when the discarded tile is of the same kind. You have to discard one of the remaining tiles in your hand, and the player next to you gets the next turn.
- Building 4 *Bodies* and 1 *Head* is not sufficient for winning by itself. You also have to achieve one or more of the 42 winning hands. For the list of winning hands, refer to [English Wikipedia](https://en.wikipedia.org/wiki/Yaku_(Japanese_mahjong)) or [NamuWiki](https://namu.wiki/w/%EB%A6%AC%EC%B9%98%EB%A7%88%EC%9E%91/%EC%97%AD). English names for the winning hands are based on English Wikipedia data.
- While you haven't called for any discarded tiles, you can declare ready if the 13 tiles in your hand is one away from building 4 *Bodies* and 1 *Head* or is one away from completing any of the winning hands. Declaring ready is counted as one of the winning "hands", which contributes to creating a unique gameplay experience in Japanese Mahjong. 
  - Declaring ready lets you open the hidden bonus tiles when calculating your score.
  - After you declare ready, you can only declare victory, make a closed quad - but only if such action does not change the list of tile you are waiting for, put aside a North (`4z`) tile, or discard the tile you have just drawn. If the first three options are not available, the system automatically discards the tile for you.
- There are 3 situations when you are prohibited from winning by discard.
  - Temporary penalty: If you decide not to win by discard although it was possible, then you cannot win by discard until your next tile draw.
  - Ready penalty: After you declare ready, if you decide not to win by discard although it was possible, then you cannot win by discard at all.
  - Discarded tile penalty: When you are one tile away from winning, if the list of tiles you are waiting for includes a tile you have discarded in the past, then you cannot win by discard at all.
- There are 2 situations when the game is aborted without a winner.
  - Abort by four quads: Automatically enforced when there are 4 quads made in total. Specifically, the abort is called when noone completes a winning hand after the player who called the 4th quad has drawn and discarded a tile. This abort is not called when 4 quads are made by the same player.
  - Abort by nine different terminal and honor tiles: Possible to call for this abort when you have 9 or more kinds of terminal and honor tiles (`1m`, `9m`, `1p`, `9p`, `1s`, `9s` and `1z` through `7z`) in your hand in your first draw. The abort cannot be called when any player before you has made a quad, called for a triplet, or has put aside a North (`4z`) tile.
- When a player has won, their score is calculated by adding the points of all winning hands satisfied, plus the points from red bonus, North (`4z`) tile bonus, and bonus tiles. The original Japanese Mahjong employs a complex scoring rule (refer to [English Wikipedia](https://en.wikipedia.org/wiki/Japanese_mahjong_scoring_rules) or [NamuWiki](https://namu.wiki/w/%EB%A6%AC%EC%B9%98%EB%A7%88%EC%9E%91/%EC%A0%90%EC%88%98) for details) that requires the computation of both *Han* and *Fu* values; in this project, the system is greatly simplified and counts 1 *Han* as 1 point. *Fu* is completely ignored.
- When the standard tile stack runs out without any player being able to win, the game ends in a draw.

If the explanation here is insufficient, please refer to other online documentation of *Mahjong Soul* or Japanese Mahjong in general.

### Omitted Features

Due to technical limitations and chosen simplifications, there are a few features in *Mahjong Soul* that were not implemented in this project.

- *Mahjong Soul* implements a time limit for each turn. Since (1) technical limitations makes the implementation of real-time countdown very difficult, and (2) the user is the only human player in this project and thus taking as much time as possible does not pose any problems, this feature was omitted.
- *Mahjong Soul* has two special rules where point transfers can happen even if the game ends in a draw. Since (1) such point transfers are meaningless because only a single round rather than a full game is implemented, and (2) the scoring rule of this project is greatly simplified from the original version and thus does not implement the concepts needed to implement these rules, these rules are deliberately omitted from the project. The two omitted rules are as follows:
  - The original Japanese Mahjong considers a concept called *Tenpai*. (See other documentations for the definition of this term.) This system transfers some small amount of points to the players who were close to victory.
  - *Mahjong Soul* implements a local rule where, if the game ends in a draw while (1) none of your discarded tiles being called for meld, and (2) all of your discarded tiles are terminal or honor tiles, you are awarded the point equal to that of *Mangan*. (See the scoring rule information for the definition of this term.)

### Fulfillment of Initial Requirements

All initial requirements stated in the proposal are fully fulfilled.
1. The game will be implemented based on the rules of 3-person Japanese Mahjong, specifically the version used in Mahjong Soul online game platform. The project will aim to replicate as many parts of the game as possible, except the scoring system which will be simplified and made appropriate for playing single rounds.

    - Fully fulfilled: see above for detailed explanation of what is implemented and what is not.

2. At the beginning of the round, the user can choose if they want to play as the dealer – “East”, the second – “South”, or the third – “West”.

    - Fully fulfilled: enter `1`, `2` or `3` to play as "East", "South" or "West" respectively.

3. All required information for the user – tiles in their hand, discarded and revealed tiles, remaining stack size, and more – will be displayed in the terminal as text. Tiles – which originally are displayed with drawings – will be displayed with their text notation used in Japan.

    - Fully fulfilled: see above for the text notation, and run the actual project to see how the information is displayed.

4. The user and two bots will take turns making moves.

    - Fully fulfilled.

5. The user will make their move – discarding, claiming, or other special moves as required by special situations – by inputting a character.

    - Fully fulfilled: the user must enter a decimal number to interact with the game.

6. The bots will operate on a set of rules that takes the given situation into account. Specifics of the playing algorithm is not determined at this point; how advanced the algorithm is will depend heavily on what is possible given the limitation of knowledge and time, which I hope will be revealed as the project moves forward.

    - Fully fulfilled: the bots consider both the tiles in their possession and all the disclosed tiles when making decisions. They discard tiles that either lets them move closer to winning hands or are not used in forming any *Bodies*. They try to prevent winning by discard by discarding tiles of the kind that were already discarded by other players. They call for quads and triplets only if such actions help the hand-building progress.
    - The project owner has played 36 games against the latest version bots, and the results are as follows: 7 player wins, 22 bot wins, 6 draws and 1 abort by nine different terminal and honor tiles. (The project owner is currently ranked at *Adept 1* in *Majhong Soul*.)

7. The round, and subsequently the program, terminates when there is a winner, when special rules are triggered, or when the tile stack runs out.

    - Fully fulfilled: see above.

### Usage of LLM

There was zero usage of LLM in this project. All 2000+ lines of code were written manually, while referencing [official F# documentation](https://learn.microsoft.com/en-gb/dotnet/fsharp/), [Fsharp.Core API Reference](https://fsharp.github.io/fsharp-core-docs/reference/) and files provided during the CS.20200 lecture.
