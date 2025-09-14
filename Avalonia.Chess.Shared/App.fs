namespace Avalonia.Chess.Shared

open Avalonia
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.FuncUI
open Avalonia.Chess
open Avalonia.Themes.Fluent

/// This is your application you can ose the initialize method to load styles
/// or handle Life Cycle events of your application
type App() =
    inherit Application()

    override this.Initialize() =
        this.RequestedThemeVariant <- Styling.ThemeVariant.Dark
        //this.Styles.Load "avares://Avalonia.Themes.Default/DefaultTheme.xaml"
        //this.Styles.Load "avares://Avalonia.Themes.Default/Accents/BaseDark.xaml"
        this.Styles.Add (FluentTheme())
        //this.Styles.Load "avares://Avalonia.Chess.Desktop/Styles.xaml"

    override this.OnFrameworkInitializationCompleted() =
        match this.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktopLifetime ->
            desktopLifetime.MainWindow <- Shell.MainWindow()
        | _ -> ()