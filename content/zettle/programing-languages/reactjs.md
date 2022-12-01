# ReactJS

A javascript framework to create user interface.

## JSX

JSX, or JavaScript XML, is an extension to the JavaScript language syntax.
It enables embeding [[html]] inside [[javascript]].

## Component

React code is made of entities called components.
Components has _props_ and render html. They can use _hooks_

## Hooks

Hooks enable side effects inside pure component. They return call-backs and read-only data.

Rules of hooks:

- Hooks should only be called at the top level (not inside loops or if statements) of a component or another hook.
- Call-back can't be called from the render context, only from external event.

The `useEffect` hook is the only way to callback a hook from the render function,
for example to fetch initial data.
