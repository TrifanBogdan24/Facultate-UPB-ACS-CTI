package command.execution.factory.commands;

import command.execution.Command;
import fileio.input.CommandInput;
import command.execution.FactoryCommand;
import command.execution.commands.GetOnlineUsers;

public final class FactoryGetOnlineUsers implements FactoryCommand {
    @Override
    public Command createCommand(final CommandInput commandInput) {
        return new GetOnlineUsers();
    }
}
