package commands;

import com.fasterxml.jackson.databind.node.ObjectNode;
import fileio.input.CommandInput;

public class CommandInvoker {

    public CommandInvoker() {

    }

    /**
     *
     * @param command           the instance of the command to run: Select / Search / Load / etc.
     *                          (instance of a class that implements CommandRunner interface)
     * @param commandInput      the input command that was read from a JSON file
     * @return                  the output command that will be written in an ouput JSON file
     */
    public ObjectNode invoke(final CommandRunner command, final CommandInput commandInput) {
        return command.execute(commandInput);
    }
}
