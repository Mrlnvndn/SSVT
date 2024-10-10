# Set the default timeout for responses (in seconds)
timeout 1.0

# Define an external channel
external 'door'

# Define a process describing the behavior of the coffee machine.
process('main') {

  # Define stimuli (input) and responses (output) the process can perform on
  # this channel.
  channel('door') {
    stimulus 'open'
    stimulus 'close'
    stimulus 'lock'
    stimulus 'unlock'

    response 'opened'
    response 'closed'
    response 'locked'
    response 'unlocked'
    response 'invalid_command'
    response 'invalid_passcode'
    response 'incorrect_passcode'
  }

  # Describe the behavior of this process. Statements are read top to bottom,
  # similar to an imperative program.

 state 'opened'
 receive 'close'
 goto 'closed'

 state 'closed'
 receive 'open'
 goto 'opened'

}