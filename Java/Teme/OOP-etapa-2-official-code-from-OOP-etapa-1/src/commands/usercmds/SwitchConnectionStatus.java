package commands.usercmds;

import app.SingletonAdmin;
import app.player.Player;
import app.user.User;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import commands.CommandRunner;
import fileio.input.CommandInput;

public final class SwitchConnectionStatus implements CommandRunner {

    public SwitchConnectionStatus() {

    }

    /**
     * SwitchConnectionStatus object node
     *
     * @param commandInput      the input command from the input/test* files
     * @return                  the ouput command, on object node that will be written
     *                          in the files of the result/ directory
     */
    @Override
    public ObjectNode execute(final CommandInput commandInput) {
        String message = swtichConnectionStatusMessage(commandInput);

        ObjectMapper objectMapper = new ObjectMapper();
        ObjectNode objectNode = objectMapper.createObjectNode();

        objectNode.put("command", commandInput.getCommand());
        objectNode.put("user", commandInput.getUsername());
        objectNode.put("timestamp", commandInput.getTimestamp());
        objectNode.put("message", message);

        return objectNode;
    }

    /**
     * SwitchConnectionStatus smessage
     *
     * @param commandInput       from the input/test* files
     * @return                   the message generated by the execution of the input command
     */
    private static String swtichConnectionStatusMessage(final CommandInput commandInput) {
        String username = commandInput.getUsername();

        SingletonAdmin admin = SingletonAdmin.getInstance();
        User user = admin.getUserByName(username);
        admin.setCurrentUser(user);

        if (user == null) {
            return ("The username " + username + " doesn't exist.");
        }

        if (!user.getClass().equals(User.class)) {
            return (username + " is not a normal user.");
        }

        Player player = user.getPlayer();

        if (user.isOnline()) {
            // entering offline mode
            user.setOnline(false);
            user.setPlayer(player);
        } else {
            // entering online mode
            user.setOnline(true);
            user.setPlayer(player);
        }

        return (username + " has changed status successfully.");
    }
}
