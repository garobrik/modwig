import {
  ContextType,
  createContext,
  useContext,
  useEffect,
  useState,
} from 'react';
import { Property } from 'csstype';

const ThemeContext = createContext({
  bg: '#f6f5f4' satisfies Property.Color,
  fg: '#c64600' satisfies Property.Color,
  blur: 15,
  distance: 5,
  intensity: 0.4,
  space: 1,
});

const useTheme = () => useContext(ThemeContext);

const Theme = ({
  children,
  ...props
}: React.PropsWithChildren<Partial<ContextType<typeof ThemeContext>>>) => (
  <ThemeContext.Provider value={{ ...useTheme(), ...props }}>
    {children}
  </ThemeContext.Provider>
);

type State = {
  pads: Control[];
  knobs: Control[];
  mode: string;
};
type Control = {
  name: string;
  bindings: Binding[];
};
type Binding = {
  name: string;
  value: number;
  displayedValue: string;
};

const usePluginConnection = () => {
  const [status, setStatus] = useState<
    'CLOSED' | 'OPEN' | 'RECEIVING' | 'ERROR'
  >('CLOSED');
  const [state, setState] = useState<State | null>(null);
  useEffect(() => {
    const socket = new WebSocket('ws://localhost:8080');
    socket.onopen = (ev) => {
      setStatus('OPEN');
    };
    socket.onmessage = (ev) => {
      setStatus('RECEIVING');
      try {
        setState(JSON.parse(ev.data));
      } catch (e: unknown) {
        setStatus('ERROR');
        socket.close();
      }
    };
    socket.onerror = (ev) => {
      setStatus('ERROR');
    };
    socket.onclose = (ev) => {
      setStatus('CLOSED');
    };
    return () => socket.close();
  }, []);

  return { status, state };
};

export const App = () => {
  const { status, state } = usePluginConnection();
  const [theme, setTheme] = useState(useTheme());
  const updateTheme = <K extends keyof typeof theme>(k: K) => ({
    value: theme[k],
    onChange: (e: React.ChangeEvent<HTMLInputElement>) =>
      setTheme((theme) => ({
        ...theme,
        [k]:
          typeof theme[k] === 'number'
            ? parseFloat(e.target.value)
            : e.target.value,
      })),
  });

  return (
    <Theme {...theme}>
      <div
        style={{ flex: 1, flexDirection: 'column', backgroundColor: theme.bg }}
      >
        <div>
          <span style={{ color: theme.fg }}>{status}</span>
          <input type="color" {...updateTheme('bg')} />
          <input type="color" {...updateTheme('fg')} />
          <input
            type="range"
            min="0"
            max="20"
            step="0.5"
            {...updateTheme('distance')}
          />
          <input
            type="range"
            min="0"
            max="30"
            step="0.5"
            {...updateTheme('blur')}
          />
          <input
            type="range"
            min="0"
            max="1"
            step="0.01"
            {...updateTheme('intensity')}
          />
          <input
            type="range"
            min="0.5"
            max="3"
            step="0.01"
            {...updateTheme('space')}
          />
        </div>
        {state && (
          <div
            style={{
              flex: 1,
              flexDirection: 'column',
              justifyContent: 'space-around',
              padding: 10,
            }}
          >
            <div
              style={{
                columnGap: '20px',
              }}
            >
              <div
                style={{
                  width: '50%',
                  display: 'grid',
                  grid: 'auto-flow / repeat(4, 1fr)',
                  gap: `${theme.space}vw`,
                }}
              >
                {state.pads.map(({ bindings: [binding] }) => (
                  <Pad text={binding?.name} />
                ))}
              </div>
              <div
                style={{
                  width: '50%',
                  display: 'grid',
                  grid: 'auto-flow / repeat(4, 1fr)',
                  gap: `${theme.space}vw`,
                }}
              >
                {state.knobs.map(({ bindings: [binding] }) => (
                  <Knob text={binding?.name} percentage={binding?.value} />
                ))}
              </div>
            </div>
          </div>
        )}
      </div>
    </Theme>
  );
};

type PadProps = {
  text: string;
};
const Pad = ({ text }: PadProps) => {
  const theme = useTheme();

  return (
    <div
      style={{
        aspectRatio: '1/1',
        borderRadius: 30,
        boxShadow: useNeuBoxShadow(),
      }}
    >
      <span
        style={{
          margin: 'auto',
          textAlign: 'center',
          fontWeight: 1000,
          fontSize: '2.5vw',
          color: theme.fg,
          overflowWrap: 'anywhere',
        }}
      >
        {text}
      </span>
    </div>
  );
};

type KnobProps = {
  text: string;
  percentage: number;
};
const Knob = ({ text, percentage }: KnobProps) => {
  const theme = useTheme();

  return (
    <div
      style={{
        position: 'relative',
        aspectRatio: '1/1',
      }}
    >
      <div
        style={{
          borderRadius: '50%',
          width: '100%',
          height: '100%',
          boxShadow: useNeuBoxShadow(),
        }}
      >
        <svg
          style={{ position: 'absolute', inset: '2%', filter: '' }}
          height="100%"
          width="100%"
        >
          <circle stroke={theme.bg} fill="transparent" strokeWidth="8%" />
        </svg>
        <div
          style={{
            position: 'absolute',
            borderRadius: '50%',
            inset: '2%',
            boxShadow: useNeuBoxShadow({
              distance: 2,
              intensity: 0.2,
              inset: true,
            }),
          }}
        >
          <div
            style={{
              position: 'absolute',
              borderRadius: '50%',
              inset: '8%',
              boxShadow: useNeuBoxShadow({ distance: 1, intensity: 0.2 }),
            }}
          ></div>
          <div
            style={{
              position: 'absolute',
              width: '100%',
              height: '100%',
              transform: `rotate(${(percentage - 0.5) * 270}deg)`,
            }}
          >
            <div
              style={{
                position: 'absolute',
                top: 0,
                left: 'calc(50% - 2px)',
                width: '3%',
                height: '8%',
                backgroundColor: theme.fg,
              }}
            />
          </div>
          <span
            style={{
              margin: 'auto',
              textAlign: 'center',
              fontWeight: 1000,
              fontSize: '2.5vw',
              color: theme.fg,
            }}
          >
            {text}
          </span>
        </div>
      </div>
    </div>
  );
};

type NeuShadowProps = {
  color: string;
  distance: number;
  blur: number;
  intensity: number;
  inset?: boolean;
};

const useNeuBoxShadow = (props?: Partial<NeuShadowProps>) =>
  neuBoxShadow({ color: useTheme().bg, ...useTheme(), ...props });

const neuBoxShadow = ({
  color,
  blur,
  intensity,
  distance,
  inset = false,
}: NeuShadowProps) => {
  const insetStr = inset ? 'inset' : '';
  return `
    ${insetStr} ${distance}px ${distance}px ${blur}px ${luminance(
    color,
    -intensity
  )}, 
    ${insetStr} ${-distance}px ${-distance}px ${blur}px ${luminance(
    color,
    intensity
  )}
`;
};

const neuSvgShadow = ({
  color,
  blur,
  intensity,
  distance,
  inset = false,
}: NeuShadowProps) => {};

const luminance = (color: string, intensity: number) => {
  const match = color.match(
    /^#([0-9a-f][0-9a-f])([0-9a-f][0-9a-f])([0-9a-f][0-9a-f])$/
  )!;

  const colors = [match[1], match[2], match[3]].map((colorStr) => {
    const color = Math.round(
      Math.min(Math.max(parseInt(colorStr, 16) * (1 + intensity), 0), 255)
    );
    return color.toString(16).padStart(2, '0');
  });

  return `#${colors.join('')}`;
};
