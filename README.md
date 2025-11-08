# Stock Market Simulator (Final)

This small Haskell project is a terminal-based stock market simulator using Brick for the UI.

Features
- Simulated random-walk stock prices
- Simple order placing (buy/sell) from the UI
- Portfolio tracking and recent trades log
- Sorting modes for market view (by id, by price asc/desc)
- Colored selected row, key help, graceful shutdown for generator threads

Keybindings
- Up / Down : Move selection
- b : Buy 1 share of selected stock
- s : Sell 1 share of selected stock
- o : Cycle sort mode (ById -> ByPriceAsc -> ByPriceDesc)
- q : Quit

Build & run
This repository uses Stack (there is a workspace task configured in VS Code). From the project root:

```powershell
stack build
stack exec <exe-name>
```

Replace `<exe-name>` with the executable name defined in your cabal/package.yaml (check `stock-sim.cabal` or package config).

Notes
- The `Final/` folder contains the program entrypoint (`Main.hs`), a `Types.hs` module, a `Controller.hs` for the simulation and order processing, `UI.hs` for rendering, and `Helpers.hs` for small utilities.
- On exit the UI sets a shutdown flag so background price-generator threads exit cleanly.
