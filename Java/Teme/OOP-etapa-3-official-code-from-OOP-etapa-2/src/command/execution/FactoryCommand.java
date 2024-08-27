package command.execution;

import fileio.input.CommandInput;

public interface FactoryCommand {
    /**
     *
     * @param       commandInput  the input command from the input/*.json files
     * @return      it will return the associated command,
     *              meant to execute the input command
     */
    Command createCommand(CommandInput commandInput);
}
