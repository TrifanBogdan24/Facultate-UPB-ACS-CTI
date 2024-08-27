package commands.playercmds;

import app.SingletonAdmin;
import app.player.PlayerStats;
import app.user.User;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import commands.CommandRunner;
import fileio.input.CommandInput;

public final class Status implements CommandRunner {

    public Status() {

    }

    /**
     * Status object node.
     *
     * @param commandInput the command input
     * @return the object node
     */
    @Override
    public ObjectNode execute(final CommandInput commandInput) {
        SingletonAdmin admin = SingletonAdmin.getInstance();
        User user = admin.getUserByName(commandInput.getUsername());

        if (user == null) {
            return null;
        }

        PlayerStats stats = user.getPlayerStats();

        ObjectMapper objectMapper = new ObjectMapper();
        ObjectNode objectNode = objectMapper.createObjectNode();

        objectNode.put("command", commandInput.getCommand());
        objectNode.put("user", commandInput.getUsername());
        objectNode.put("timestamp", commandInput.getTimestamp());
        objectNode.put("stats", objectMapper.valueToTree(stats));

        return objectNode;
    }

}
