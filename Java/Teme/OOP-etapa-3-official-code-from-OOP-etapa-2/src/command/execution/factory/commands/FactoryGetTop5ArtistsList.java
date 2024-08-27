package command.execution.factory.commands;

import command.execution.Command;
import fileio.input.CommandInput;
import command.execution.FactoryCommand;
import command.execution.commands.GetTop5ArtistList;

public final class FactoryGetTop5ArtistsList implements FactoryCommand {
    @Override
    public Command createCommand(final CommandInput commandInput) {
        return new GetTop5ArtistList();
    }
}
