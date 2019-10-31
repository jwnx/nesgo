import React, { useEffect, useRef, useState } from 'react'
import Modal from "react-modal"
import styled, { css } from 'styled-components'
import { Events } from '@wailsapp/runtime'

function useEventListener(eventName, handler, element = window) {
  const savedHandler = useRef()
  useEffect(() => {
    savedHandler.current = handler
  }, [handler])
  useEffect(
    () => {
      const isSupported = element && element.addEventListener
      if (!isSupported) {
        return
      }
      const eventListener = event => savedHandler.current(event)
      element.addEventListener(eventName, eventListener)
      return () => {
        element.removeEventListener(eventName, eventListener)
      }
    },
    [eventName, element] // Re-run if eventName or element changes
  )
}

const Container = styled.div`
  display: flex;
  flex-flow: column;
  background-color: #2b324d;
  text-align: center;
  min-height: 100vh;
  align-items: ${props => (props.center ? "center" : "stretch")};
  justify-content: ${props => (props.center ? "center" : "flex-start")};
`

const Logo = styled.div`
  font-size: calc(10px + 2vmin);
  color: #c69b49;
`

const Button = styled.button`
  background: transparent;
  border-radius: 3px;
  border: 2px solid #2b324d;
  color: #a3afaf;
  margin: 0.5em 1em;
  padding: 0.25em 1em;

  :hover {
    color: #c69b49;
  }

  ${props =>
    props.primary
      ? css`
          background: #906462;
          color: #fbfafa;
          :hover {
            border: 2px solid #906462;
          }
        `
      : css`
          :hover {
            border-bottom: 2px solid #906462;
          }
        `}
`

const Menu = styled.div`
  display: flex;
  flex-flow: row;
  order: -1;
  margin: 15px;
  color: #fbfafa;

  & nav,
  aside {
    flex: 0 0 20vw;
  }
  & nav {
    text-align: left;
    margin-left: 2vw;
    cursor: default;
    & > span {
      font-weight: bold;
      :hover {
        color: #a3afaf;
      }
    }
  }
  & aside {
    text-align: right;
    margin-right: 2vw;
    & > button {
      background: transparent;
      border-radius: 3px;
      border: 2px solid #fbfafa;
      color: #fbfafa;
      :hover {
        background: #fbfafa;
        color: #2b324d;
      }
    }
  }
  & p {
    flex: 1;
  }
`

const Close = styled.a`
  color: #777;
  font: 14px/100% arial, sans-serif;
  position: absolute;
  right: 3vw;
  text-decoration: none;
  top: 2vw;
  :after {
    content: "✖";
  }
`

const Controls = styled.table`
  border-collapse: collapse;
  border-spacing: 0;
  line-height: 1.6;
  margin-top: 5vw;
  margin-left: 20vw;
  margin-right: 20vw;

  th,
  td {
    width: 20vw;
    border-bottom: 1px solid #ddd;
  }
`

Modal.setAppElement('#app');

function ReactModalAdapter({ className, modalClassName, ...props }) {
  return (
    <Modal className={modalClassName} portalClassName={className} {...props} />
  )
}

const StyledModal = styled(ReactModalAdapter).attrs({
  overlayClassName: "Overlay",
  modalClassName: "Modal"
})`
  .Modal {
    position: absolute;
    top: 40px;
    left: 40px;
    right: 40px;
    bottom: 40px;
    background: rgba(43, 50, 77, 1);
    color: #c69b49;
    display: flex;
    flex-flow: column;
    text-align: center;
    justify-content: center;
    align-items: center;
  }
  .Overlay {
    position: fixed;
    top: 0;
    left: 0;
    right: 0;
    bottom: 0;
    background: rgba(251, 250, 250, 0.75);
  }
`

const Img = styled.img`
  flex: 100%;
`

const Frame = () => {
  const [frame, setFrame] = useState(null)
  Events.On("new_frame", setFrame)
  return (
    <Img alt='frame' src={`data:image/png;base64,${frame}`} />
  )
}

function handleController(e) {
  const press = window.backend.GUI.Press
  switch (e.key.toLowerCase()) {
    case "x": press(0); break
    case "z": press(1); break
    case "a": press(2); break
    case "enter": press(3); break
    case "arrowup": press(4); break
    case "arrowdown": press(5); break
    case "arrowleft": press(6); break
    case "arrowright": press(7); break
  }
}

const Display = ({ unstart, title }) => {
  useEventListener("keydown", handleController)
  const [isOpen, setIsOpen] = useState(false)
  const done = () => window.backend.GUI.Stop().then(unstart)
  return (
    <Container>
      <Menu>
        <nav>
          <span onClick={done}>‹ Back</span>
        </nav>
        <p>{title}</p>
        <aside>
          <button onClick={() => setIsOpen(true)}>Controls</button>
        </aside>
        <StyledModal isOpen={isOpen} onRequestClose={() => setIsOpen(false)}>
          <h2>Controls</h2>
          <Close onClick={() => setIsOpen(false)} />
          <Controls>
            <thead>
              <tr>
                <th>Button</th>
                <th>Player 1</th>
              </tr>
            </thead>
            <tbody>
              <tr>
                <td>Left</td>
                <td>Left</td>
              </tr>
              <tr>
                <td>Right</td>
                <td>Right</td>
              </tr>
              <tr>
                <td>Up</td>
                <td>Up</td>
              </tr>
              <tr>
                <td>Down</td>
                <td>Down</td>
              </tr>
              <tr>
                <td>A</td>
                <td>X</td>
              </tr>
              <tr>
                <td>B</td>
                <td>Z</td>
              </tr>
              <tr>
                <td>Start</td>
                <td>Enter</td>
              </tr>
              <tr>
                <td>Select</td>
                <td>A</td>
              </tr>
            </tbody>
          </Controls>
        </StyledModal>
      </Menu>
      <Frame />
    </Container>
  )
}

const Rom = ({ rom, setRom }) => {
  const [maybeLoadRom, setMaybeLoadRom] = useState(false)
  return (
    <Button
      onMouseOver={() => setMaybeLoadRom(true)}
      onMouseOut={() => setMaybeLoadRom(false)}
      onClick={() => window.backend.GUI.OpenRom().then(setRom)}
    >
      {maybeLoadRom ? "Load..." : rom}
    </Button>
  )
}

function withLoading(Component) {
  return ({ setRom, rom }) => {
    const [started, setStarted] = useState(false)
    const handleStartClick = () =>
      window.backend.GUI.Start().then(
        () => setStarted(true),
        () => setRom("invalid ROM"))
    if (started) {
      return <Component unstart={() => setStarted(false)} title={rom} />
    }
    return (
      <Container center>
        <Logo>
          <h1>
            NESGO
          </h1>
        </Logo>
        <Button primary onClick={handleStartClick}>
          Start
        </Button>
        <Rom setRom={setRom} rom={rom} />
      </Container>
    )
  }
}

const App = () => {
  const [rom, setRom] = useState("")
  window.backend.GUI.SelectedRom().then(setRom)
  const LoadedGame = withLoading(Display)
  return (
    <LoadedGame rom={rom} setRom={setRom} />
  )
}

export default App