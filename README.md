# SmartEvent

A decentralized event ticketing and management platform built on Stacks blockchain using Clarity smart contracts.

## Overview

SmartEvent is a blockchain-based solution that revolutionizes event ticketing by leveraging NFT technology. The platform enables event organizers to create token-gated events with tiered access, issue verifiable tickets, track attendance, and control ticket resales to prevent scalping.

## Features

- **NFT Tickets**: Each ticket is a unique digital asset owned by the attendee
- **Tiered Access**: Create multiple ticket tiers with different prices and benefits
- **Attendance Tracking**: Verify and record attendance on-chain
- **Resale Controls**: Set resale permissions and price caps to prevent scalping
- **Event Management**: Create, configure, and manage events through smart contracts

## Smart Contract Architecture

The SmartEvent platform is built around a primary Clarity smart contract that handles all core functionality:

### Data Structures

- **Events**: Stores event details including name, description, date, venue, and ticket limits
- **Ticket Tiers**: Defines different ticket categories with varying prices and benefits
- **Tickets**: Represents individual NFT tickets with ownership and usage information
- **Attendance**: Tracks event check-ins and participation

### Key Functions

#### For Event Organizers

- `create-event`: Create a new event with customizable parameters
- `add-ticket-tier`: Define ticket tiers with different prices and benefits
- `check-in`: Record attendance and validate tickets at the venue
- `end-event`: Close an event after completion

#### For Attendees

- `purchase-ticket`: Buy a ticket for a specific event and tier
- `list-ticket-for-sale`: List a ticket on the secondary marketplace
- `buy-resale-ticket`: Purchase a ticket from another user

#### Read-Only Functions

- `get-event`: Retrieve event details
- `get-ticket-tier`: View information about a specific ticket tier
- `get-ticket`: Get details about a specific ticket
- `get-attendance`: See attendance records for an event
- `owns-ticket`: Check if a user owns a ticket for an event

## Usage Examples

### Creating an Event

```clarity
(contract-call? .smart-event create-event 
  "Blockchain Summit 2023" 
  "Annual conference for blockchain enthusiasts" 
  u1672531200 
  "Tech Convention Center" 
  u500 
  true 
  u1000000000)
```

### Adding Ticket Tiers

```clarity
(contract-call? .smart-event add-ticket-tier 
  u1 
  "VIP Access" 
  u100000000 
  u50 
  "Front row seating, exclusive networking event, speaker meet & greet")
```

### Purchasing a Ticket

```clarity
(contract-call? .smart-event purchase-ticket u1 u1)
```

### Checking In Attendees

```clarity
(contract-call? .smart-event check-in u1 u5)
```

## Error Handling

The contract includes comprehensive error handling with specific error codes:

- `ERR-NOT-AUTHORIZED (u100)`: User doesn't have permission for the operation
- `ERR-EVENT-NOT-FOUND (u101)`: The specified event doesn't exist
- `ERR-TICKET-NOT-FOUND (u102)`: The specified ticket doesn't exist
- `ERR-TICKET-ALREADY-USED (u103)`: Attempt to use a ticket that's already been redeemed
- `ERR-RESALE-NOT-ALLOWED (u104)`: Ticket resale is disabled for this event
- `ERR-INVALID-PRICE (u105)`: Resale price exceeds the maximum allowed
- `ERR-EVENT-ENDED (u106)`: The event has already concluded
- `ERR-SOLD-OUT (u107)`: No more tickets available for purchase

## Development

### Prerequisites

- [Clarinet](https://github.com/hirosystems/clarinet) - Clarity development environment
- [Stacks Wallet](https://www.hiro.so/wallet) - For testing and interacting with the contract

### Local Development

1. Clone the repository
   ```bash
   git clone https://github.com/Queensamuel99999/SmartEvent.git
   cd SmartEvent
   ```

2. Initialize Clarinet project (if not already done)
   ```bash
   clarinet new
   ```

3. Test the contract
   ```bash
   clarinet test
   ```

4. Deploy to testnet
   ```bash
   clarinet deploy --testnet
   ```

## Future Enhancements

- Integration with SIP-009 NFT standard for improved interoperability
- Multi-signature event management for collaborative organizing
- Enhanced ticket verification with QR codes
- Event discovery marketplace
- Integration with physical access control systems
- Support for recurring events and event series

## Security Considerations

- The contract implements access controls to ensure only authorized users can perform sensitive operations
- Ticket ownership is tracked and verified on-chain
- Resale controls help prevent ticket scalping and fraud
- Attendance tracking provides an immutable record of participation

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add some amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## Contact

Project Link: [https://github.com/Queensamuel99999/SmartEvent](https://github.com/Queensamuel99999/SmartEvent)

---

Built with ❤️ on the Stacks blockchain.
