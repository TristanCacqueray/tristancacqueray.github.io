<!-- DoNotFormat -->
<bind tag="containerClass"><ema:metadata><value var="template.layout.note.containerClass" /></ema:metadata></bind>
<!-- DoNotFormat -->

<apply template="base">
  <bind tag="head-main">
    <link rel="stylesheet" href="${ema:emanoteStaticLayerUrl}/inverted-tree.css" />
  </bind>
  <bind tag="body-main">
    <div class="${containerClass}">
      <div class="mt-2 md:mt-4">
        <apply template="components/note-uptree" />

        <div class="relative md:shadow-2xl md:mb-8">
          <div class="absolute -top-6 right-1 md:right-0 flex flex-row items-center justify-center">
            <a title="Search (Ctrl+K)" class="cursor-pointer"
              onclick="window.emanote.stork.toggleSearch()">
              <apply template="components/stork/stork-icon" />
            </a>
          </div>
          <div class="flex-1 w-full overflow-x-auto bg-white">
            <main class="px-4 py-4">
              <apply template="components/note-title" />

              <script src="https://cdn.midirus.com/script/webamp.bundle.min.js" integrity="sha384-6L8JnEU7pQtPpwiqWZK5nqUzAwWxN9nAJyOinBQXjuG8DpqaSjIiEaCyOzay0YDi" crossorigin="anonymous"></script>
              <div style="min-height:730px">
                <br /><br /><br /><br />
<div id="webamp-player"></div>
<br /><br /><br /><br /><br />
<div id="info" style="width:500px">
  <apply template="components/note-body" />
</div>
<script src="/static/audio-player.js"></script>
                <p style="position:absolute; top:120px; font-size:small; width:200px">
                  Free sound to use for all<br />
                  your Creative Common or<br />
                  GPL projects.<br />
                  No credit is required,<br />
                  but always appreciated.
                </p>
                <div class="place-items-center flex flex-col" style="position:absolute; top:98px;right:23px;">
<svg height="40px" width="40px" version="1.1" id="headphone" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" viewBox="0 0 512 512"  xml:space="preserve">
<g>
	<path class="st0" d="M175.898,335.919c-3.812-25.66-27.679-43.357-53.33-39.546c-25.659,3.778-43.374,27.671-39.57,53.322
		l18.12,122.236c3.804,25.65,27.671,43.365,53.33,39.554c25.651-3.795,43.357-27.662,39.562-53.33L175.898,335.919z"/>
	<path class="st0" d="M389.438,296.373c-25.651-3.811-49.518,13.886-53.33,39.546l-18.121,122.236
		c-3.786,25.667,13.911,49.535,39.571,53.33c25.65,3.812,49.518-13.903,53.33-39.554l18.12-122.236
		C432.811,324.044,415.088,300.151,389.438,296.373z"/>
	<path class="st0" d="M506.813,166.683l-11.106-21.231c-22.6-43.187-56.364-79.478-97.625-105.07
		C356.864,14.799,307.997-0.009,256.003,0C204-0.009,155.132,14.799,113.914,40.382c-41.26,25.592-75.025,61.883-97.616,105.07
		L5.192,166.683c-4.301,8.224-1.14,18.391,7.099,22.701l7.649,3.99c-7.952,24.315-12.322,50.262-12.322,77.247
		c0.042,31.753,7.353,62.948,15.594,90.551c8.231,27.476,17.57,51.707,21.965,67.402l35.683-10.074
		c-5.29-18.593-14.384-42.004-22.152-67.959c-7.767-25.87-14.055-53.98-14.004-79.919c0-20.791,3.034-40.838,8.621-59.803
		l1.436,0.777c8.24,4.285,18.391,1.116,22.692-7.116l11.105-21.239c15.788-30.198,39.512-55.679,68.332-73.538
		c28.828-17.859,62.567-28.136,99.112-28.144c36.536,0.008,70.284,10.286,99.112,28.144c28.813,17.858,52.536,43.34,68.324,73.538
		l11.106,21.239c4.302,8.231,14.461,11.4,22.693,7.116l1.437-0.777c5.586,18.965,8.628,39.012,8.628,59.803
		c0.042,25.938-6.237,54.049-14.012,79.919c-7.759,25.955-16.861,49.366-22.152,67.959l35.683,10.074
		c4.395-15.695,13.734-39.926,21.966-67.402c8.24-27.602,15.559-58.798,15.592-90.551c0-26.986-4.37-52.932-12.322-77.247
		l7.649-3.99C507.945,185.074,511.107,174.907,506.813,166.683z"/>
</g>
</svg>
<div style="font-size:28px">🖢</div>
                </div>
              </div>

              <apply template="components/metadata" />
              <apply template="/templates/hooks/note-end" />
            </main>
          </div>
        </div>
        <apply template="components/footer" />
      </div>
    </div>
  </bind>
</apply>
