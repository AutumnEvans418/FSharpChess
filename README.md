# FSharpChess

FSharpChess is a chess engine and GUI application written in F#. It features a modular chess logic library and a cross-platform Avalonia UI frontend.

![chess game](assets/image.png)

## Features
- Chess engine with move validation and AI opponent
- Avalonia-based GUI for interactive play
- Modular design for easy extension
- Unit tests for core logic

## Projects
- **Chess**: Core chess logic and engine
- **Avalonia.Chess**: GUI frontend using Avalonia
- **Tests**: Unit tests for chess logic

## Getting Started

### Prerequisites
- [.NET 8 SDK](https://dotnet.microsoft.com/download)

### Build and Run
1. Clone the repository:
   ```sh
   git clone https://github.com/chrisevans9629/FSharpChess.git
   cd FSharpChess
   ```
2. Build the solution:
   ```sh
   dotnet build
   ```
3. Run the GUI application:
   ```sh
   dotnet run --project Avalonia.Chess/Avalonia.Chess.fsproj
   ```

### Running Tests
```sh
 dotnet test Tests/Tests.fsproj
```

## Usage
- Launch the GUI and start a new game
- Play against the AI or another human
- View move history and game status

## Acknowledgements
- [Avalonia UI](https://avaloniaui.net/) for cross-platform GUI
- F# community for language and library support
