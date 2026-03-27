# BRB/USD Trading Platform

A simulated cryptocurrency exchange built in OCaml. Users can create accounts, deposit funds, and place buy/sell orders for a fictional token (BRB) traded against USD. Orders are matched automatically using a price-time priority engine with support for partial fills.

**Authors:** Anthony Fine (alf223) & Nick Johnson (NMJ37)

---

## Features

- Interactive CLI with colorized terminal output
- Order book matching engine (buy/sell, partial fills, price-time priority)
- JSON-based persistent storage — databases saved in `data/`
- ASCII order book visualization
- Multi-user account management with deposit/withdraw
- OUnit2 test suite (glass-box, ~320 lines of tests)

---

## Tech Stack

| Component | Technology |
|-----------|-----------|
| Language | OCaml |
| Build system | Dune 3.4 |
| JSON I/O | `yojson` + `ppx_deriving_yojson` |
| Terminal colors | `ANSITerminal` |
| Testing | `ounit2` |

---

## Project Structure

```
.
├── bin/
│   ├── main.ml        # REPL, UI, command parsing
│   └── order.ml       # ASCII order book graph
├── src/
│   ├── database.ml    # Trading engine & state management
│   └── database.mli   # Public interface
├── test/
│   └── test.ml        # OUnit2 test suite
├── data/
│   ├── empty.json         # Empty database
│   ├── 3usersexample.json # Sample database with 3 users and orders
│   └── order_example.json # Order-focused test data
├── Makefile
├── dune-project
└── Install.md
```

---

## Installation

Requires [opam](https://opam.ocaml.org/) and OCaml.

```bash
opam install ounit ANSITerminal yojson ppx_deriving_yojson
```

---

## Building & Running

```bash
# Build
make build

# Run the trading platform (recommended)
make play

# Run tests
make test

# Open OCaml REPL with library loaded
make utop

# Build documentation
make doc
```

Or run directly with dune:

```bash
dune exec bin/main.exe
```

---

## Usage

When you start the platform, you'll be prompted to create or load a database (think of it as a market instance), then log in or create a user account.

```
# Start the platform
make play

# At the prompt:
> new_database mymarket     # Create a new market database
> login Nick                # Log in as an existing user
> new_user Alice            # Or create a new account

# Once logged in (Account menu):
> deposit usd 1000          # Add 1000 USD to your account
> deposit brb 500           # Add 500 BRB to your account
> withdraw usd 200          # Remove 200 USD
> order                     # Enter the order flow (buy or sell)
> graph                     # View the ASCII order book visualization
> home                      # Return to the login screen
> quit                      # Save and exit
```

**Order flow:**
1. Choose `buy` or `sell`
2. Enter the rate (USD per BRB, e.g. `0.50`)
3. Enter the amount of BRB
4. The engine automatically matches against existing opposite orders

**Admin menu:**
```
> admin        # View all users and balances
```

---

## Sample Data

Three example databases are included in `data/`:

- `empty.json` — blank market, no users or orders
- `3usersexample.json` — Tony, Nick, and Anthony with balances and an open buy order
- `order_example.json` — empty database for building test scenarios

Load one at startup with `load` or start fresh with `new_database`.

---

## How the Matching Engine Works

Orders are sorted by price priority:
- **Buy orders:** highest rate first (willing to pay more = matched first)
- **Sell orders:** lowest rate first (cheapest ask = matched first)

When a buy order's rate >= a sell order's rate, they match:
- Funds transfer between accounts immediately
- Partial fills are supported — unmatched remainder stays in the book
- Orders are removed when fully filled

---

## Running Tests

```bash
make test
```

Tests cover:
- User creation and balance management
- Deposit/withdraw operations
- Basic order placement
- Complex order matching (partial fills, multiple price levels)
- JSON serialization/deserialization
