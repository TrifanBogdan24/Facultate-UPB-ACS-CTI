package command.execution.commands;

import app.SingletonAdmin;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import command.execution.Command;
import fileio.input.CommandInput;

import java.util.List;

public class GetTop5Albums implements Command {
    public GetTop5Albums() {

    }

    /**
     * Gets top 5 album list.
     *
     * @param commandInput the command input
     * @return the top 5 album list
     */
    @Override
    public ObjectNode execute(final CommandInput commandInput) {
        SingletonAdmin admin = SingletonAdmin.getInstance();
        List<String> albums = admin.getTop5AlbumList();

        ObjectMapper objectMapper = new ObjectMapper();
        ObjectNode objectNode = objectMapper.createObjectNode();

        objectNode.put("command", commandInput.getCommand());
        objectNode.put("timestamp", commandInput.getTimestamp());
        objectNode.put("result", objectMapper.valueToTree(albums));

        return objectNode;
    }
}
