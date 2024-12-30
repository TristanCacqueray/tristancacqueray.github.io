<div class="flex-1 w-full bg-white">
  <main class="px-4 py-4">
    <apply template="note-title" />
    <ema:has:toc>
      <div class="md:grid md:gap-4 md:grid-cols-8">
        <div class="md:col-span-6">
          <my-body />
        </div>
        <div class="hidden md:block md:col-span-2 md:border-l">
          <apply template="toc" />
        </div>
      </div>
      <else />
      <my-body />
    </ema:has:toc>
    <div class="flex flex-col lg:flex-row lg:space-x-2">
      <apply template="timeline" />
      <apply template="backlinks" />
    </div>
    <apply template="metadata" />
    <apply template="/templates/hooks/note-end" />
  </main>
</div>
