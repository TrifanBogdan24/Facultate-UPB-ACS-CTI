package commands.playercmds;

import app.SingletonAdmin;
import app.audio.LibraryEntry;
import app.player.Player;
import app.searchBar.Filters;
import app.searchBar.SearchBar;
import app.user.Artist;
import app.user.Host;
import app.user.User;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import commands.CommandRunner;
import fileio.input.CommandInput;

import java.util.ArrayList;
import java.util.List;

public class Search implements CommandRunner {

    private static final Integer FIVE = 5;

    public Search() {

    }

    /**
     * Search object node.
     *
     * @param commandInput the command input
     * @return the object node
     */
    @Override
    public ObjectNode execute(final CommandInput commandInput) {
        String username = commandInput.getUsername();

        SingletonAdmin admin = SingletonAdmin.getInstance();
        User user = admin.getUserByName(username);
        admin.setCurrentUser(user);

        String message = new String();
        ArrayList<String> results = new ArrayList<>();

        if (user == null) {
            message = "The username " + username + " doesn't exist.";
        } else if (!user.isOnline()) {
            message = username + " is offline.";
        } else {
            results = searchResults(commandInput);
            message = "Search returned " + results.size() + " results";
        }

        ObjectMapper objectMapper = new ObjectMapper();
        ObjectNode objectNode = objectMapper.createObjectNode();

        objectNode.put("command", commandInput.getCommand());
        objectNode.put("user", commandInput.getUsername());
        objectNode.put("timestamp", commandInput.getTimestamp());
        objectNode.put("message", message);
        objectNode.put("results", objectMapper.valueToTree(results));

        return objectNode;
    }

    /**
     * Search array list.
     *
     * @param commandInput the command input
     * @return the array list
     */
    public static ArrayList<String> searchResults(final CommandInput commandInput) {

        Filters filters = new Filters(commandInput.getFilters());
        String type = commandInput.getType();

        SingletonAdmin admin = SingletonAdmin.getInstance();
        User user = admin.getCurrentUser();
        user.setTimestampLastPlayedFile(null);


        Player player = user.getPlayer();
        SearchBar searchBar = user.getSearchBar();
        player.stop();

        searchBar.clearSelection();
        searchBar.setLoadedLibraryEntry(null);

        user.setLastSearched(true);
        ArrayList<String> results = new ArrayList<>();


        if (type.equals("song") || type.equals("podcast")
                || type.equals("playlist") || type.equals("album")) {

            List<LibraryEntry> libraryEntries = searchBar.search(filters, type);

            for (LibraryEntry libraryEntry : libraryEntries) {
                results.add(libraryEntry.getName());
            }

            searchBar.setSearchedLibraryEntry(libraryEntries);

        } else if (type.equals("artist")) {
            results = searchArtists(filters);
            searchBar.setSearchedArtists(results);

        } else if (type.equals("host")) {
            results = searchHosts(filters);
            searchBar.setSearchedHosts(results);

        } else {
            results.add("Error : Invalid search type" + type + ".");
        }

        searchBar.setLastSearchType(commandInput.getType());

        user.setPlayer(player);
        user.setSearchBar(searchBar);

        return results;
    }

    /**
     *
     * @param filters       the filters provided by an input command
     * @return              maximum 5 artists that match the filter
     */
    public static ArrayList<String> searchArtists(final Filters filters) {
        SingletonAdmin admin = SingletonAdmin.getInstance();

        ArrayList<String> resultedArtists = new ArrayList<>();

        for (Artist artist: admin.getAllArtists()) {

            if (filters.getName() != null
                    && !artist.getUsername().startsWith(filters.getName())) {
                continue;
            }

            resultedArtists.add(artist.getUsername());

            if (resultedArtists.size() >= FIVE) {
                break;
            }
        }

        return resultedArtists;
    }

    /**
     *
     * @param filters       the filters provideed by an input command
     * @return              maximum 5 hosts that match filters
     */
    public static ArrayList<String> searchHosts(final Filters filters) {
        SingletonAdmin admin = SingletonAdmin.getInstance();

        ArrayList<String> resultedHosts = new ArrayList<>();

        for (Host host: admin.getAllHosts()) {

            if (!host.getUsername().startsWith(filters.getName())) {
                continue;
            }

            resultedHosts.add(host.getUsername());

            if (resultedHosts.size() >= FIVE) {
                break;
            }
        }

        return resultedHosts;
    }

}
