package commands.playercmds;

import app.SingletonAdmin;
import app.player.Player;
import app.user.User;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import commands.CommandRunner;
import fileio.input.CommandInput;

public final class PlayPause implements CommandRunner {

    public PlayPause() {

    }

    /**
     * PlayPause object node
     *
     * @param commandInput      the input command from the input/test* files
     * @return                  the ouput command, on object node that will be written
     *                          in the files of the result/ directory
     */
    @Override
    public ObjectNode execute(final CommandInput commandInput) {
        String message = playPauseMessage(commandInput);

        ObjectMapper objectMapper = new ObjectMapper();
        ObjectNode objectNode = objectMapper.createObjectNode();

        objectNode.put("command", commandInput.getCommand());
        objectNode.put("user", commandInput.getUsername());
        objectNode.put("timestamp", commandInput.getTimestamp());
        objectNode.put("message", message);

        return objectNode;
    }


    /**
     * PlayPause message
     *
     * @param commandInput       from the input/test* files
     * @return                   the message generated by the execution of the input command
     */
    public static String playPauseMessage(final CommandInput commandInput) {
        String username = commandInput.getUsername();

        SingletonAdmin admin = SingletonAdmin.getInstance();
        User user = admin.getUserByName(username);
        admin.setCurrentUser(user);


        if (user == null) {
            return "The username " + username + " doesn't exist.";
        }

        user.setTimestampLastPlayedFile(null);

        if (!user.isOnline()) {
            return username + " is offline.";
        }

        Player player = user.getPlayer();

        if (player.getCurrentAudioFile() == null) {
            return "Please load a source before attempting to pause or resume playback.";
        }

        player.pause();
        user.setPlayer(player);
        user.setTimestampLastPlayedFile(commandInput.getTimestamp());


        if (player.getPaused()) {
            return "Playback paused successfully.";
        } else {
            return "Playback resumed successfully.";
        }
    }
}
