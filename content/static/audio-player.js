// Copyright © 2024 Tristan de Cacqueray
// SPDX-License-Identifier: GPL-3.0-or-later

function setupPlayer() {
  const app = document.getElementById("webamp-player");
  const webamp = new Webamp({
    zIndex: 99999,
    windowLayout: {
      main: { position: { top: 120, left: -800 } },
      equalizer: { position: { top: 235, left: -800 } },
      playlist: { position: { top: 120, left: -525 } },
    },
  });

  return webamp.renderWhenReady(app).then(() => {
    // Increase playlist size
    webamp.store.dispatch({
      type: "WINDOW_SIZE_CHANGED",
      windowId: "playlist",
      // Get the value with: webamp.store.getState().windows.genWindows.playlist.size
      size: [2, 23],
    });

    // Help with debug...
    globalThis.webamp = webamp;

    return webamp;
  });
}

globalThis.cdnHost = "https://cdn.midirus.com";

function getAudio() {
  return fetch(new Request(cdnHost + "/medias.json"), {cache: "no-cache"}).then((
    response,
  ) => response.json());
}

function setURLParam(k, v) {
  const urlParams = new URLSearchParams(window.location.search);
  if (v) urlParams.set(k, v);
  else urlParams.delete(k);
  const path = "audio" + (urlParams.size > 0 ? "?" + urlParams.toString() : "");
  window.history.replaceState(null, null, path);
}

// convert an audio file into a track
function audio2track(audioFile, pos) {
  return {
    url: audioFile.url,
    duration: audioFile.duration,
    metaData: {
      artist: (pos < 9 ? "  " : pos < 99 ? " " : "") +
        audioFile.date.slice(0, 7) +
        " | " +
        audioFile.album,
      title: audioFile.title,
    },
  };
}

function isAudio(media) {
  return media.fmt.indexOf("flac") > -1 || media.fmt.indexOf("mp3") > -1;
}

function toTitleCase(str) {
  return str.replace(
    /\w\S*/g,
    (text) => text.charAt(0).toUpperCase() + text.substring(1).toLowerCase(),
  );
}

Promise.all([setupPlayer(), getAudio()]).then(([player, data]) => {
  const urlParams = new URL(window.location.toString()).searchParams;

  // Traverse the data and prepare the playlists
  const playlists = {};
  const releases = new Set();
  const addToPlaylist = (name, media) => {
    name = name == "best" ? "Best Of" : toTitleCase(name);
    if (!playlists[name]) {
      playlists[name] = [media];
    } else {
      playlists[name].push(media);
    }
  };
  const audioFiles = data.flatMap((release) =>
    release.medias.filter(isAudio).map((media) => {
      if (media.fmt && media.fmt.indexOf("flac") > -1) {
        media.urlFLAC = cdnHost + media.path + ".flac";
      }
      media.url = cdnHost + media.path + ".mp3";
      media.duration = media.len / 1000;
      media.album = release.name;
      releases.add(release.name);
      addToPlaylist(release.name, media);
      media.tags.forEach((t) => addToPlaylist(t, media));
      return media;
    })
  ).sort((a, b) => a.date > b.date ? -1 : (a.date == b.date ? 0 : 1));

  // Start with shuffle on
  player.store.dispatch({ type: "TOGGLE_SHUFFLE" });
  player.store.dispatch({ type: "SET_EQ_OFF" });

  // Setup track list
  let tracks;
  let tracksFiles;
  const loadTracks = (files) => {
    // console.log("Loading playlist", files.length);
    tracksFiles = files;
    tracks = tracksFiles.map(audio2track);
    player.setTracksToPlay(tracks);
    player.pause();
  };
  const pickBest = () => {
    setURLParam("track", null);
    const bests = [];
    tracksFiles.forEach((track, pos) => {
      if (track.tags.indexOf("best") > -1) bests.push(pos);
    });
    return bests.length == 0
      ? 0
      : bests[Math.floor(Math.random() * bests.length)];
  };

  // Keep track of the playing track position
  let currentPos = -1;
  let currentTitle = "";
  player.onTrackDidChange((track) => {
    if (track) {
      currentPos = tracksFiles.findIndex((t) => t.url == track.url);
      currentTitle = track.metaData.title;
      console.log("Track changed to", currentPos, track);
    }
  });

  const queueTrack = (pos) => {
    if (pos > -1) {
      // Find the internal position for the PLAY_TRACK dispatch
      const trackOrder = player.store.getState().playlist.trackOrder;
      if (pos < trackOrder.length) {
        const trackid = trackOrder[pos];
        player.store.dispatch({ type: "PLAY_TRACK", id: trackid });
      } else {
        console.error("Out of bound pos", pos, trackOrder);
      }
    }
  };

  // Load a playlist
  const loadPlaylist = (name) => {
    const files = playlists[name];
    if (files) {
      loadTracks(files);
      return true;
    } else {
      return false;
    }
  };

  // Setup playlist
  const playlistsElt = document.getElementById("my-playlist");
  const releasesElt = document.getElementById("my-release");
  const clearBG = (pelt) => {
    for (let i = 0; i < pelt.children.length; i++) {
      pelt.children[i].classList.remove(window.playlistSelectedNames);
    }
  };

  const addPlaylist = (pelt, name) => {
    const elt = document.createElement("div");
    elt.role = "button";
    elt.className = window.playlistClassNames;
    if (name == urlParams.get("playlist")) {
      elt.classList.add(window.playlistSelectedNames);
    }
    elt.innerText = name;
    elt.onclick = () => {
      if (name == "Everything") {
        loadTracks(audioFiles);
        setURLParam("playlist", null);
      } else {
        loadPlaylist(name);
        setURLParam("playlist", name);
      }
      clearBG(playlistsElt);
      clearBG(releasesElt);
      elt.classList.add(window.playlistSelectedNames);
      queueTrack(pickBest());
    };
    pelt.appendChild(elt);
  };

  addPlaylist(playlistsElt, "Everything");
  addPlaylist(playlistsElt, "Best Of");
  Object.keys(playlists).forEach((n) => {
    if (!releases.has(n) && n != "Best Of") addPlaylist(playlistsElt, n);
  });
  releases.forEach((n) => addPlaylist(releasesElt, n));

  // Handle download button
  const dlBtn = document.getElementById("dl-btn");
  dlBtn.onclick = () => {
    // Hide download button
    dlBtn.classList.add("hidden");
  };

  // Load initial playlist
  if (!loadPlaylist(urlParams.get("playlist"))) {
    loadTracks(audioFiles);
    playlistsElt.children[0].classList.add(window.playlistSelectedNames);
  }

  // Load initial track
  const urlTrack = urlParams.get("track");
  let initialPos = -1;
  if (urlTrack) {
    const name = urlTrack.replace("+", " ");
    initialPos = tracksFiles.findIndex((t) => t.title == name);
    // console.log("Found track", name, "at pos", initialPos);
  }
  if (initialPos === -1) {
    initialPos = 0;
  }
  queueTrack(initialPos);

  // A background thread to track the currently playing track
  let lastPos;
  const checkPlaying = () => {
    if (player.media._context.state === "running" && currentPos != lastPos) {
      lastPos = currentPos;
      const audioFile = tracksFiles[currentPos];
      // console.log("Currently playing", audioFile);
      // Show download button
      dlBtn.classList.remove("hidden");
      dlBtn.href = audioFile.urlFLAC;
      // Update location
      setURLParam("track", audioFile.title);
    }
    setTimeout(checkPlaying, 1000);
  };
  checkPlaying();

  player.onWillClose((cancel) => {
    if (!window.confirm("Are you sure you want to close Webamp?")) {
      cancel();
    }
  });
  player.onMinimize(() => {
    cancel();
  });
});
