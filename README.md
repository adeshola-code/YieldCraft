# YieldCraft Protocol Aggregator

## Overview

YieldCraft is a smart contract designed to aggregate multiple yield protocols, allowing users to efficiently manage and optimize their asset deposits across different DeFi platforms.

## Features

- Aggregate multiple yield protocols
- Automatic selection of best-yielding protocol
- Secure asset management
- Flexible protocol addition and management
- Reward calculation and claiming
- Platform fee mechanism

## Smart Contract Capabilities

### Core Functions

- `deposit-to-best-protocol`: Deposit assets into the highest-yielding protocol
- `withdraw-from-protocol`: Withdraw assets from a specific protocol
- `claim-rewards`: Claim accumulated rewards
- `add-protocol`: Add new protocols to the aggregator
- `update-protocol-stats`: Update protocol rates and statistics

### Key Components

- Protocol management with active/inactive states
- User deposit tracking
- Dynamic APY and TVL calculation
- Token validation
- Platform fee calculation

## Error Handling

The contract includes comprehensive error codes for scenarios like:
- Unauthorized actions
- Invalid protocols
- Insufficient balances
- Slippage limits
- Protocol activation status

## Configuration Parameters

- Minimum deposit: 1,000,000 smallest token units
- Maximum slippage: 0.5%
- Platform fee: 0.1%
- Maximum protocols: 50

## Security Considerations

- Owner-only protocol management
- Token and protocol address validation
- Deposit and withdrawal checks
- Rewards calculation safeguards

## Deployment Requirements

- Stacks blockchain environment
- SIP-010 compatible tokens
- Protocol trait implementation

## Usage Example

1. Add protocols
2. Deposit assets
3. Automatically get best yield
4. Claim rewards
5. Withdraw assets

## Potential Integrations

- Lending protocols
- Staking platforms
- Yield farming strategies

## Limitations

- Limited to 50 protocols
- Requires ongoing manual protocol stats updates

## Contributing

- Protocol additions
- Performance optimizations
- Additional validation logic
