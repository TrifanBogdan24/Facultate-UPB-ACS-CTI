package command.execution.factory.commands;

import command.execution.Command;
import fileio.input.CommandInput;
import command.execution.FactoryCommand;
import command.execution.commands.GetTop5Playlists;

public final class FactoryGetTop5Playlists implements FactoryCommand {
    @Override
    public Command createCommand(final CommandInput commandInput) {
        return new GetTop5Playlists();
    }
}
