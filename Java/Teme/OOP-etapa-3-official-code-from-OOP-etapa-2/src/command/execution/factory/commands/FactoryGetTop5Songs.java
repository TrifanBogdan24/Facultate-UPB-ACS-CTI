package command.execution.factory.commands;

import command.execution.Command;
import fileio.input.CommandInput;
import command.execution.FactoryCommand;
import command.execution.commands.GetTop5Songs;

public final class FactoryGetTop5Songs implements FactoryCommand {
    @Override
    public Command createCommand(final CommandInput commandInput) {
        return new GetTop5Songs();
    }
}
