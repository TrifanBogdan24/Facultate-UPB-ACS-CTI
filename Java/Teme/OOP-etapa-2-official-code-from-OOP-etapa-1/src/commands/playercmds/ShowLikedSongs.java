package commands.playercmds;

import app.SingletonAdmin;
import app.audio.Files.AudioFile;
import app.user.User;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import commands.CommandRunner;
import fileio.input.CommandInput;

import java.util.ArrayList;

public final class ShowLikedSongs implements CommandRunner {

    public ShowLikedSongs() {

    }


    /**
     * ShowLikedSongs object node
     *
     * @param commandInput      the input command from the input/test* files
     * @return                  the ouput command, on object node that will be written
     *                          in the files of the result/ directory
     */
    @Override
    public ObjectNode execute(final CommandInput commandInput) {
        SingletonAdmin admin = SingletonAdmin.getInstance();
        User user = admin.getUserByName(commandInput.getUsername());
        admin.setCurrentUser(user);

        ArrayList<String> songs = showPreferredSongs();

        ObjectMapper objectMapper = new ObjectMapper();
        ObjectNode objectNode = objectMapper.createObjectNode();

        objectNode.put("command", commandInput.getCommand());
        objectNode.put("user", commandInput.getUsername());
        objectNode.put("timestamp", commandInput.getTimestamp());
        objectNode.put("result", objectMapper.valueToTree(songs));

        return objectNode;
    }

    /**
     * ShowLikedSongs message
     *
     * @return                   the message generated by the execution of the input command
     */
    private static ArrayList<String> showPreferredSongs() {
        SingletonAdmin admin = SingletonAdmin.getInstance();
        User currentUser = admin.getCurrentUser();

        if (currentUser == null) {
            return (new ArrayList<>());
        }

        currentUser.setTimestampLastPlayedFile(null);

        ArrayList<String> results = new ArrayList<>();
        for (AudioFile audioFile : currentUser.getLikedSongs()) {
            results.add(audioFile.getName());
        }

        return results;
    }
}
