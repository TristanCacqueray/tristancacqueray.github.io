<ema:metadata>
  <section
    class="flex flex-wrap items-end justify-center my-4 space-x-2 space-y-2 font-mono text-sm">
    <with var="tags">
      <!-- FIXME: The use of -/tags is wrong, because we should use routeUrl using Ema's encoder
        Perhaps Emanote should inject tagMetas with urls.
      -->
      <a title="Tag" class="px-1 bg-gray-100 rounded hover:bg-gray-50 hover:text-${theme}-500"
        href="-/tags/${value}${ema:urlStrategySuffix}">
        <!-- DoNotFormat -->
        #<value />
        <!-- DoNotFormat -->
      </a>
    </with>
  </section>
  <div class="flex items-center justify-center mt-2">
    <a class="text-gray-300 hover:text-${theme}-600 text-sm" title="Edit this page on GitHub"
      href="https://github.com/TristanCacqueray/TristanCacqueray.github.io/edit/main/docs/${ema:note:source-path}">
      <svg xmlns="http://www.w3.org/2000/svg" class="h-6 w-6" fill="none" viewBox="0 0 24 24"
        stroke="currentColor">
        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2"
          d="M11 5H6a2 2 0 00-2 2v11a2 2 0 002 2h11a2 2 0 002-2v-5m-1.414-9.414a2 2 0 112.828 2.828L11.828 15H9v-2.828l8.586-8.586z" />
      </svg>
    </a>
    <p class="text-sm text-gray-600 mx-2">
      This work is licensed under a <a class="hover:underline" href="http://creativecommons.org/licenses/by/4.0/">Creative Commons Attribution 4.0 International License</a>
    </p>
  </div>
</ema:metadata>
