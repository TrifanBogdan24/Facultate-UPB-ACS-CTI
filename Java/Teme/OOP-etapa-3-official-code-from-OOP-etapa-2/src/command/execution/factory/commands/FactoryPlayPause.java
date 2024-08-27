package command.execution.factory.commands;

import command.execution.Command;
import fileio.input.CommandInput;
import command.execution.FactoryCommand;
import command.execution.commands.PlayPause;

public final class FactoryPlayPause implements FactoryCommand {
    @Override
    public Command createCommand(final CommandInput commandInput) {
        return new PlayPause();
    }
}
