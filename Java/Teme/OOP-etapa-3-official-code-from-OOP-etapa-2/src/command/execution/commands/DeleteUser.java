package command.execution.commands;

import app.SingletonAdmin;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import command.execution.Command;
import fileio.input.CommandInput;

public class DeleteUser implements Command {
    public DeleteUser() {

    }

    /**
     * Delete user object node.
     *
     * @param commandInput the command input
     * @return the object node
     */
    @Override
    public ObjectNode execute(final CommandInput commandInput) {
        SingletonAdmin admin = SingletonAdmin.getInstance();
        String message = admin.deleteUser(commandInput.getUsername());

        ObjectMapper objectMapper = new ObjectMapper();
        ObjectNode objectNode = objectMapper.createObjectNode();

        objectNode.put("command", commandInput.getCommand());
        objectNode.put("user", commandInput.getUsername());
        objectNode.put("timestamp", commandInput.getTimestamp());
        objectNode.put("message", message);

        return objectNode;
    }
}
