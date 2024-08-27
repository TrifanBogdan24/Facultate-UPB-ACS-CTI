package command.execution.factory.commands;

import command.execution.Command;
import fileio.input.CommandInput;
import command.execution.FactoryCommand;
import command.execution.commands.Next;

public final class FactoryNext implements FactoryCommand {
    @Override
    public Command createCommand(final CommandInput commandInput) {
        return new Next();
    }
}
