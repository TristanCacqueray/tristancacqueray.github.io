# Zuul

[Zuul](https://zuul-ci.org) is a [[continuous-integration]] system that features cross-project [gating](https://zuul-ci.org/docs/zuul/discussion/gating.html).
This is a process that prevents invalid changes from being merged. This keeps the mainline of development open and working for all developers, and only when a change is confirmed to work without disruption it is merged.

My main contributions to Zuul are:

- Support `zuul.d` configuration directory, which is useful to enable part of the configuration to be generated.
- Nodepool drivers abstract interface so that Zuul can use arbritary resources providers.
- Amazon EC2, Kubernetes and OpenShift drivers. Zuul can now run workload transparently on virtual instances such as OpenStack Nova or Amazon EC2, as well as Container.
- Rest API and Patternfly React user interface.
- A 3D rendering of the logo ([glsl source](https://opendev.org/zuul/zuul-website-media/commit/65e841766116a46c352023271f836faa9ca78950)):

:::{.flex .justify-center .items-center .mb-8}
<video loop autoplay>
  <source src="https://zuul-ci.org/media/zuul.mp4" type="video/mp4" />
</video>
