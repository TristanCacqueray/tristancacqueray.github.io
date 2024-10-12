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

// For local dev, run `npx http-server --cors`
globalThis.cdnHost = "https://cdn.midirus.com" // "http://10.0.0.88:8081"
function getAudio() {
  const ms = Date.now();
  return fetch(new Request(cdnHost + "/audio.json?ts=" + ms)).then((response) =>
    response.json(),
  );
}

Promise.all([setupPlayer(), getAudio()]).then(([player, data]) => {
  // Add url and trackID for easy access
  data.files.forEach((f, pos) => {
    f.url = cdnHost + "/audio/" + f.path + ".mp3";
    f.pos = pos;
  });
  const getFile = (url) => data.files.filter((f) => f.url == url)[0];

  // Start with shuffle on
  player.store.dispatch({ type: "TOGGLE_SHUFFLE" });
  player.store.dispatch({ type: "SET_EQ_OFF" });

  // Setup webamp track list
  const tracks = data.files.map((audioFile) => ({
    url: audioFile.url,
    duration: audioFile.nfo.meta.length / 1000,
    metaData: {
      artist: audioFile.album,
      title: audioFile.title,
    },
  }));
  player.setTracksToPlay(tracks);

  // Find initial track
  let firstPick;
  const urlParams = new URL(window.location.toString()).searchParams;
  const urlTrack = urlParams.get("track");
  if (urlTrack) {
    const name = urlTrack.replace(":", "/");
    const audioFile = data.files.filter((f) => f.path == name)[0];
    if (audioFile) firstPick = audioFile.pos;
  }
  if (!firstPick) {
    const topTracks = data.files
      .filter((f) => f.nfo.meta.rating > 1)
      .map((f) => f.pos);
    firstPick = topTracks[Math.floor(Math.random() * topTracks.length)];
  }
  player.store.dispatch({ type: "PLAY_TRACK", id: firstPick });

  player.onTrackDidChange((track) => {
    if (track) {
      const audioFile = getFile(track.url);
      if (audioFile.pos != firstPick)
       window.history.replaceState(
        null,
        null,
        "audio?track=" + audioFile.path.replace("/", ":"),
      );
    }
  });
  player.onWillClose((cancel) => {
    if (!window.confirm("Are you sure you want to close Webamp?")) {
      cancel();
    }
  });
  player.onMinimize(() => {
    cancel();
  });
});
