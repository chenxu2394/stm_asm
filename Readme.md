# An Archived Repository for the Assembly Code for STM Control

This repository contains the assembly code for controlling a Scanning Tunneling Microscope (STM) via the Freescale DSP56309EVM board. For interface details, see the corresponding [C code repository](https://www.github.com/chenxu2394/stm). During STM operations, the STM software sends a character via the RS232 to the DSP board. Each character corresponds to a specific command that the DSP executes in a blocking manner. After execution, the DSP sends the resultant data back to the STM software, readying the system for the next operation cycle.

## Available Commands

The code starts with a detailed list of commands. Each command is activated by sending a specific character from the STM software. For example:

- `d`: Delay command that pauses the operation allowing time-based operations to be handled synchronously.
- `i`: Input command that reads data from specified channels on the STM.

For a complete list of commands, refer to the start of the `stm.asm` file.
