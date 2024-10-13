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

globalThis.cdnHost = window.location.hostname == "127.0.0.1" ?
  // For local dev, run `npx http-server --cors`
  "http://10.0.0.88:8081" :
  "https://cdn.midirus.com"
function getAudio() {
  const ms = Date.now();
  return fetch(new Request(cdnHost + "/audio.json?ts=" + ms)).then((response) =>
    response.json(),
  );
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
      artist:
        (pos < 9 ? "  " : pos < 99 ? " " : "") +
        audioFile.release.slice(0, 7) +
        " | " +
        audioFile.album,
      title: audioFile.title,
    },
  };
}

Promise.all([setupPlayer(), getAudio()]).then(([player, data]) => {
  const urlParams = new URL(window.location.toString()).searchParams;

  // Enrich the audio.json data with full URLs and duration
  data.files.forEach((f, pos) => {
    f.url = cdnHost + "/audio/" + f.path + ".mp3";
    f.urlFLAC = cdnHost + "/audio/" + f.path + ".flac";
    f.duration = f.nfo.meta.length / 1000;
  });

  // Start with shuffle on
  player.store.dispatch({ type: "TOGGLE_SHUFFLE" });
  player.store.dispatch({ type: "SET_EQ_OFF" });

  // Setup track list
  let tracks;
  let tracksFiles;
  const loadTracks = (audioFiles) => {
    // console.log("Loading playlist", audioFiles.length);
    tracksFiles = audioFiles;
    tracks = audioFiles.map(audio2track);
    player.setTracksToPlay(tracks);
    player.pause();
  };
  const pickBest = () => {
    setURLParam("track", null);
    const bests = [];
    tracksFiles.forEach((track, pos) => {
      if (track.nfo.meta.rating > 1 && track.title != currentTitle) bests.push(pos);
    });
    return bests.length == 0
      ? -1
      : bests[Math.floor(Math.random() * bests.length)];
  };

  // Keep track of the playing track position
  let currentPos = -1;
  let currentTitle = ""
  player.onTrackDidChange((track) => {
    if (track) {
      currentPos = tracksFiles.findIndex((t) => t.url == track.url);
      currentTitle = track.metaData.title
      console.log("Track changed to", currentPos, track)
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
    if (name == "Best Of") {
      loadTracks(data.files.filter(f => f.nfo.meta.rating > 3))
    } else {
      const playlist = data.albums.find(p => p.name == name) || data.playlists.find(p => p.name == name)
      if (playlist)
        loadTracks(playlist.sounds.map((idx) => data.files[idx]));
      else
        return false
    }
    return true
  }

  // Setup playlist
  const playlists = document.getElementById("my-playlist");
  const addPlaylist = (name) => {
    const elt = document.createElement("div");
    elt.role = "button";
    elt.className =
      "text-slate-800 flex w-full items-center rounded-md p-3 transition-all hover:bg-slate-100 focus:bg-slate-100 active:bg-slate-100 whitespace-nowrap";
    if (name == urlParams.get("playlist"))
      elt.classList.add("bg-slate-200")
    elt.innerText = name;
    elt.onclick = () => {
      if (name == "Everything") {
        loadTracks(data.files);
        setURLParam("playlist", null);
      } else {
        loadPlaylist(name)
        setURLParam("playlist", name);
      }
      for (let i = 0; i < playlists.children.length; i++)
        playlists.children[i].classList.remove("bg-slate-200");
      elt.classList.add("bg-slate-200");
      queueTrack(pickBest());
    };
    playlists.appendChild(elt);
  };
  addPlaylist("Everything")
  addPlaylist("Best Of")
  data.playlists.forEach(n => addPlaylist(n.name));
  data.albums.forEach(n => addPlaylist(n.name));

  // Handle download button
  const dlBtn = document.getElementById("dl-btn");
  dlBtn.onclick = () => {
    // Hide download button
    dlBtn.classList.add("hidden");
  };

  // Load initial playlist
  if (!loadPlaylist(urlParams.get("playlist"))) {
    loadTracks(data.files);
    playlists.children[0].classList.add("bg-slate-200")
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
    initialPos = pickBest();
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
