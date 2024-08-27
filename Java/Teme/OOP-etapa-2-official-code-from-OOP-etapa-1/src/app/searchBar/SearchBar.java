package app.searchBar;


import app.SingletonAdmin;
import app.audio.LibraryEntry;
import lombok.Data;

import java.util.ArrayList;
import java.util.List;

import static app.searchBar.FilterUtils.filterByAlbum;
import static app.searchBar.FilterUtils.filterByArtist;
import static app.searchBar.FilterUtils.filterByFollowers;
import static app.searchBar.FilterUtils.filterByGenre;
import static app.searchBar.FilterUtils.filterByLyrics;
import static app.searchBar.FilterUtils.filterByName;
import static app.searchBar.FilterUtils.filterByOwner;
import static app.searchBar.FilterUtils.filterByPlaylistVisibility;
import static app.searchBar.FilterUtils.filterByReleaseYear;
import static app.searchBar.FilterUtils.filterByTags;
import static app.searchBar.FilterUtils.filterByDescription;

/**
 * The type Search bar.
 */

@Data
public final class SearchBar {
    private static final Integer MAX_RESULTS = 5;

    private List<LibraryEntry> searchedLibraryEntry;
    private String user;
    private String lastSearchType;
    private LibraryEntry lastSelectedLibraryEntry;
    private LibraryEntry loadedLibraryEntry;

    private ArrayList<String> searchedArtists;
    private ArrayList<String> searchedHosts;

    private String lastSelectedArtist;
    private String lastSelectedHost;


    /**
     * Instantiates a new Search bar.
     *
     * @param user the user
     */
    public SearchBar(final String user) {
        this.searchedLibraryEntry = new ArrayList<>();
        this.user = user;

        this.searchedArtists = new ArrayList<>();
        this.searchedHosts = new ArrayList<>();
    }

    /**
     * Clear selection.
     */
    public void clearSelection() {
        clearSearch();

        this.lastSelectedLibraryEntry = null;
        this.lastSelectedArtist = null;
        this.lastSelectedHost = null;
    }

    /**
     *
     */
    public void clearSearch() {
        this.lastSearchType = null;
        this.searchedArtists = null;
        this.searchedHosts = null;

        this.loadedLibraryEntry = null;
    }

    /**
     * Search list.
     *
     * @param filters the filters
     * @param type    the type
     * @return the list
     */
    public ArrayList<LibraryEntry> search(final Filters filters, final String type) {
        SingletonAdmin admin = SingletonAdmin.getInstance();
        List<LibraryEntry> entries;

        switch (type) {
            case "song":
                entries = new ArrayList<>(admin.getSongs());

                if (filters.getName() != null) {
                    entries = filterByName(entries, filters.getName());
                }

                if (filters.getAlbum() != null) {
                    entries = filterByAlbum(entries, filters.getAlbum());
                }

                if (filters.getTags() != null) {
                    entries = filterByTags(entries, filters.getTags());
                }

                if (filters.getLyrics() != null) {
                    entries = filterByLyrics(entries, filters.getLyrics());
                }

                if (filters.getGenre() != null) {
                    entries = filterByGenre(entries, filters.getGenre());
                }

                if (filters.getReleaseYear() != null) {
                    entries = filterByReleaseYear(entries, filters.getReleaseYear());
                }

                if (filters.getArtist() != null) {
                    entries = filterByArtist(entries, filters.getArtist());
                }

                break;
            case "playlist":
                entries = new ArrayList<>(admin.getPlaylists());

                entries = filterByPlaylistVisibility(entries, user);

                if (filters.getName() != null) {
                    entries = filterByName(entries, filters.getName());
                }

                if (filters.getOwner() != null) {
                    entries = filterByOwner(entries, filters.getOwner());
                }

                if (filters.getFollowers() != null) {
                    entries = filterByFollowers(entries, filters.getFollowers());
                }

                break;
            case "podcast":
                entries = new ArrayList<>(admin.getPodcasts());

                if (filters.getName() != null) {
                    entries = filterByName(entries, filters.getName());
                }

                if (filters.getOwner() != null) {
                    entries = filterByOwner(entries, filters.getOwner());
                }

                break;

            case "album":
                entries = new ArrayList<>(admin.getAlbums());

                if (filters.getName() != null) {
                    entries = filterByName(entries, filters.getName());
                }

                if (filters.getOwner() != null) {
                    entries = filterByOwner(entries, filters.getOwner());
                }

                if (filters.getDescription() != null) {
                    entries = filterByDescription(entries, filters.getDescription());
                }

                break;
            default:
                entries = new ArrayList<>();
        }

        while (entries.size() > MAX_RESULTS) {
            entries.remove(entries.size() - 1);
        }

        this.searchedLibraryEntry = entries;
        this.lastSearchType = type;
        return (new ArrayList<>(entries));
    }

    /**
     * Select library entry.
     *
     * @param itemNumber the item number
     * @return the library entry
     */
    public LibraryEntry select(final Integer itemNumber) {
        if (this.searchedLibraryEntry.size() < itemNumber) {
            searchedLibraryEntry.clear();

            return null;
        } else {
            lastSelectedLibraryEntry =  this.searchedLibraryEntry.get(itemNumber - 1);
            searchedLibraryEntry.clear();

            return lastSelectedLibraryEntry;
        }
    }

    /**
     *  anulates the load field
     */
    public void unloadSource() {
        this.loadedLibraryEntry = null;
    }

}
