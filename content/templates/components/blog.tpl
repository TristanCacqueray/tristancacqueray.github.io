<div class="flex-1 w-full bg-white">
  <main class="px-4 py-4">
    <h1
      class="flex flex-col justify-center mb-1 p-3 bg-${theme}-100 text-5xl font-extrabold text-black rounded"
    >
      <a class="z-40 tracking-tighter text-center">
        <ema:note:title />
      </a>
    </h1>
    <ema:has:toc>
      <div class="md:grid md:gap-4 md:grid-cols-8">
        <div class="md:col-span-6">
          <apply template="note-body" />
        </div>
        <div class="hidden md:block md:col-span-2 md:border-l">
          <time
            class="text-sm text-right font-mono text-gray-600 block -mt-6 rounded px-1 mb-2 bg-${theme}-100"
          >
            <ema:note:date />
          </time>
          <apply template="toc" />
        </div>
      </div>
      <else />
        <time
          class="text-sm text-right font-mono text-gray-600 block -mt-6 rounded px-1 mb-6 bg-${theme}-100"
        >
          <ema:note:date />
        </time>
      <apply template="note-body" />
    </ema:has:toc>
    <div class="flex flex-col lg:flex-row lg:space-x-2">
      <apply template="timeline" />
      <apply template="backlinks" />
    </div>
    <apply template="metadata" />
    <apply template="/templates/hooks/note-end" />
  </main>
</div>
