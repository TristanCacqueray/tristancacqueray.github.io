<bind tag="containerClass"
  ><ema:metadata
    ><value var="template.layout.note.containerClass" /></ema:metadata
></bind>

<apply template="base">
  <bind tag="head-main">
    <link
      rel="stylesheet"
      href="${ema:emanoteStaticLayerUrl}/inverted-tree.css"
    />
  </bind>
  <bind tag="body-main">
    <div class="${containerClass}">
      <div class="mt-2 md:mt-4">
        <apply template="components/note-uptree" />

        <div class="relative md:shadow-2xl md:mb-8">
          <div
            class="absolute -top-6 right-1 md:right-0 flex flex-row items-center justify-center"
          >
            <a
              title="Search (Ctrl+K)"
              class="cursor-pointer"
              onclick="window.emanote.stork.toggleSearch()"
            >
              <apply template="components/stork/stork-icon" />
            </a>
          </div>
          <div class="flex-1 w-full overflow-x-auto bg-white">
            <main class="px-4 py-4">
              <apply template="components/note-title" />
              <div class="grid grid-cols-12">
                <aside class="bg-indigo-50 col-span-3 px-2 py-1 -mt-4">
                  <div class="sticky top-0">
                    <apply template="/templates/components/toc-snippets" />
                  </div>
                </aside>
                <article class="overflow-auto col-span-9 mx-2">
                  <apply template="/templates/components/org" />
                </article>
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
