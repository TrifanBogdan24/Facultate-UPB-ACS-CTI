package command.execution.commands;

import app.SingletonAdmin;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import command.execution.Command;
import fileio.input.CommandInput;

import java.util.List;

public class GetAllUsers implements Command {
    public GetAllUsers() {

    }

    /**
     * Gets all users.
     *
     * @param commandInput the command input
     * @return the all users
     */
    @Override
    public ObjectNode execute(final CommandInput commandInput) {
        SingletonAdmin admin = SingletonAdmin.getInstance();
        List<String> users = admin.getAllUsers();

        ObjectMapper objectMapper = new ObjectMapper();
        ObjectNode objectNode = objectMapper.createObjectNode();

        objectNode.put("command", commandInput.getCommand());
        objectNode.put("timestamp", commandInput.getTimestamp());
        objectNode.put("result", objectMapper.valueToTree(users));

        return objectNode;
    }
}
