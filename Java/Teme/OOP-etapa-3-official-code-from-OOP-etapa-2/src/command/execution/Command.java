package command.execution;

import com.fasterxml.jackson.databind.node.ObjectNode;
import fileio.input.CommandInput;



public interface Command {
    /**
     *
     * @param commandInput      input command, that will be interpreted and executed
     * @return                  ObjectNode output command
     */
    ObjectNode execute(CommandInput commandInput);
}
