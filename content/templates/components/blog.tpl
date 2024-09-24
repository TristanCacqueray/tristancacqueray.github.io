<div class="flex-1 w-full bg-white">
  <main class="px-4 py-4">
    <h1
      class="flex flex-col justify-center mb-1 p-3 bg-${theme}-100 text-5xl font-extrabold text-black rounded">
      <a class="z-40 tracking-tighter text-center">
        <ema:note:title />
      </a>
    </h1>
    <time class="text-sm text-right font-mono text-gray-600 block mb-2">
      <ema:note:date />
    </time>
    <article class="overflow-auto">
      <apply template="/templates/components/pandoc" />
    </article>
    <div class="flex flex-col lg:flex-row lg:space-x-2">
      <apply template="timeline" />
      <apply template="backlinks" />
    </div>
    <apply template="metadata" />
    <apply template="/templates/hooks/note-end" />
  </main>
</div>
